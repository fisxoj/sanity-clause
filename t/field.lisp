(defpackage sanity-clause/test.field
  (:use #:cl
	#:rove
	#:sanity-clause.field
        #:alexandria
        #:cl-arrows))

(in-package #:sanity-clause/test.field)

(eval-when (:load-toplevel :compile-toplevel)
  (defvar +manual-only-fields+ '(member-field
				 nested-field)
    "Symbols of field types that require extra data to exist, like :class:`member-field`, which requires a set of symbols as an initarg."))


(defmacro with-test-fields (() &body body)
  "Generates a lexical environment where one of each kind of field (classes with a -FIELD suffix) are defined and bound to their name.

E.g. (let ((string-field (make-field 'string))
           ...)
        [BODY])"

  `(let (,@(let (field-bindings)
	     (do-external-symbols (symbol (find-package :sanity-clause.field) field-bindings)
	       (when (and (str:ends-with-p "-FIELD" (symbol-name symbol))
			  (not (member symbol +manual-only-fields+))
			  (find-class symbol nil))
		 (push `(,symbol (make-instance ',symbol)) field-bindings)))))
     ,@body))


(defclass inventory ()
  ((potato-count :reader potato-count-of :initarg :count)))

(deftest test-find-field
  (ok (sanity-clause.field:find-field :string)
      "can find a field by symbol.")
  (ok (sanity-clause.field:find-field "real")
      "can find a field by string."))

(deftest test-get-value
  (testing "missing values"
    (let ((default-field (make-instance 'field :default 3 :attribute 'potato-count))
	  (field (make-instance 'field :attribute 'potato-count))
	  (clos-inventory (make-instance 'inventory))
	  (plist-inventory '())
	  (alist-inventory))

      (ok (= (get-value default-field clos-inventory) 3)
	  "Default values work for clos objects.")

      (ok (eq (get-value field clos-inventory) :missing)
	  "A missing value with no default is :missing for clos objects.")

      (ok (= (get-value default-field plist-inventory) 3)
	  "Default values work for plists.")

      (ok (eq (get-value field plist-inventory) :missing)
	  "A missing value with no default is :missing for plists.")

      (ok (= (get-value default-field alist-inventory) 3)
	  "Default values work for alists.")

      (ok (eq (get-value field alist-inventory) :missing)
	  "A missing value with no default is :missing for alists."))))


(deftest test-deserialize
  (with-test-fields ()
    (testing "String field"
      (ok (deserialize string-field "potato")
	  "deserializes a string.")

      (ok (signals (deserialize string-field 4.5) 'conversion-error)
	  "signals an error when given a number."))

    (testing "Integer field"
      (ok (deserialize integer-field "4")
	  "deserializes a string.")

      (ok (deserialize integer-field 4)
	  "deserializes an integer.")

      (ok (signals (deserialize integer-field 4.5) 'conversion-error)
	  "signals an error when given a float.")

      (ok (signals (deserialize integer-field "garbage") 'conversion-error)
	  "signals an error when given a garbage string."))

    (testing "Real field"
      (ok (deserialize real-field "3.1415")
	  "deserializes PI as a string ;).")

      (ok (deserialize real-field 3.1415)
	  "deserializes PI as a float ;).")

      (ok (signals (deserialize real-field "garbage") 'conversion-error)
	  "signals an error when given a garbage string.")

      (ok (signals (deserialize real-field :garbage) 'conversion-error)
	  "signals an error when given a non-string and non-real datatype."))

    (testing "Member field"
      (let ((member-field (make-field 'member :members '(:yam :yukon :idaho))))
	(ok (deserialize member-field "YaM")
	    "deserializes a string with different case into its union member.")

	(ok (deserialize member-field :idaho)
	    "deserializes a keyword that is a union member.")

	(ok (signals (deserialize member-field :woodear) 'conversion-error)
	    "signals an error for a keyword that isn't a member of the union.")

	(ok (signals (deserialize member-field "washington") 'conversion-error)
	    "signals an error for a string that isn't a member of the union.")

	(ok (signals (deserialize member-field 3) 'conversion-error)
	    "signals an error for a non-string-like type.")))

    (testing "Boolean field"
      (ok (eq (deserialize boolean-field "on") t)
	  "converts a truthy string to t.")

      (ok (signals (deserialize boolean-field "wumbo") 'conversion-error)
	  "signals an error for an uncertain value."))

    (testing "Timestamp field"
      (ok (typep (deserialize timestamp-field "2006-06-06TZ") 'local-time:timestamp)
	  "converts values to timestamps from LOCAL-TIME.")

      (ok (signals (deserialize timestamp-field "pizza") 'conversion-error)
	  "throws a conversion error on badly formatted timestamp."))))


(deftest test-validate
  (testing "validation-error"
    ;; Two disjoint criteria to generate two validation errors
    (let ((test-field (make-field 'integer :validator (list (lambda (v) (sanity-clause.validator:int v :max -3))
							    (lambda (v) (sanity-clause.validator:int v :min 3))))))
      (handler-case
	  (progn (validate test-field "woo.com")
		 (fail "invalid data didn't raise a validation-error."))

	(validation-error (e)
	  (ok (= (length (error-messages-of e)) 2)
	      "had more than 0 error messages.")))))

  (with-test-fields ()
    (testing "Integer field"
      (ok (eq (validate integer-field 4) nil)
	  "validates an integer."))

    (testing "Email field"
      (ok (null (validate email-field "yam@potato.spud"))
	  "validates an email address.")

      (ok (signals (validate email-field "wooo.com") 'validation-error)
	  "raises an error when given an invalid email address."))

    (testing "String field"
      (ok (null (validate string-field "some string"))
	  "accepts any string."))

    (testing "Constant field"
      (let ((constant-string-field (make-field 'constant :constant "potato" :test 'string=))
	    (constant-number-field (make-field 'constant :constant 3 :test '=))
	    (constant-keyword-field (make-field 'constant :data-key 'potato-type :constant :russet)))

	(ok (null (validate constant-string-field "potato"))
	    "validates a correct string with string=.")

	(ok (signals (validate constant-string-field "worm") 'validation-error)
	    "raises an error for a value that isn't the constant according to string=.")

	(ok (null (validate constant-number-field 3))
	    "validates a constant number value with =.")

	(ok (signals (validate constant-number-field 5) 'validation-error)
	    "raises an error for a value that isn't = to a constant.")

	(ok (signals (sanity-clause.field:validate constant-keyword-field :armadillo) 'sanity-clause.field:validation-error)
	    "raises an error if the value doesn't match the constant value")))

    (testing "UUID field"
      (ok (null (validate uuid-field "74827715-C657-4122-B6CF-63E3FA700FF6"))
	  "accepts a uuid string.")

      (ok (signals (validate uuid-field "74827715-GGGG-4122-B6CF-63E3FA700FF6") 'validation-error)
	  "raises validation errors for something that looks like a uuid, but has forbidden 'G's in it."))))
