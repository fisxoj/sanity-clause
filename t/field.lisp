(defpackage t.sanity-clause.field
  (:use #:cl
	#:rove
	#:sanity-clause.field
        #:alexandria
        #:cl-arrows))

(in-package #:t.sanity-clause.field)

(defvar +manual-only-fields* '(member-field
			       nested-field)
  "Symbols of field types that require extra data to exist, like :class:`member-field`, which requires a set of symbols as an initarg.")


(defmacro with-test-fields (() &body body)
  "Generates a lexical environment where one of each kind of field (classes with a -FIELD suffix) are defined and bound to their name.

E.g. (let ((string-field (make-field 'string))
           ...)
        [BODY])"

  `(let (,@(let (field-bindings)
	     (do-external-symbols (symbol (find-package :sanity-clause.field) field-bindings)
	       (when (and (str:ends-with-p "-FIELD" (symbol-name symbol))
			  (not (member symbol +manual-only-fields*))
			  (find-class symbol nil))
		 (push `(,symbol (make-instance ',symbol)) field-bindings)))))
     ,@body))


(defclass inventory ()
  ((potato-count :reader potato-count-of :initarg :count)))

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
	    "signals an error for a non-string-like type.")))))


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

    (testing "UUID field"
      (ok (null (validate uuid-field "74827715-C657-4122-B6CF-63E3FA700FF6"))
	  "accepts a uuid string.")

      (ok (signals (validate uuid-field "74827715-GGGG-4122-B6CF-63E3FA700FF6") 'validation-error)
	  "raises validation errors for something that looks like a uuid, but has forbidden 'G's in it."))))


(deftest test-field-types
  (testing "constant field"
    (let ((constant-field (make-field 'constant :data-key 'potato-type :constant :russet)))

      (ok (eq (sanity-clause.field:get-value constant-field '(:lemon 4 :potato-type :armadillo)) :russet)
	  "Always returns the constant value.")

      (ok (signals (sanity-clause.field:validate constant-field :armadillo)
              'sanity-clause.field:validation-error)
          "Raises an error if the value doesn't match the constant value")))

  (testing "member field"
    (let ((potato-field (make-field 'member :members '(:yam :yukon :idaho))))

      (ok (eq (deserialize potato-field "Yam") :yam)
          "finds a string that is STRING-EQUAL to a member keyword.")

      (ok (signals (deserialize potato-field "carrot")
              'sanity-clause.field:conversion-error)
          "raises an error when the value can't be found in the union.")))

  (testing "integer field"
    (let ((age-field (make-field 'integer
				 :validator (lambda (v) (sanity-clause.validator:int v :min 0)))))

      (ok (= (deserialize age-field "4") 4)
          "deserializes text to integers.")

      (ok (signals (validate age-field -3)
              'validation-error)
          "raises an invalidation error when the minimum value validator fails.")))

  (testing "boolean field"
    (let ((happy-field (make-field 'boolean))
          (default-field (make-field 'boolean :default t)))

      (ok (eq (deserialize happy-field "on") t)
          "converts truthy string to t.")

      (ok (signals (deserialize default-field :potato)
	      'conversion-error)
          "uses a default value when the data isn't in the source datastructure.")))

  (testing "timestamp field"
    (let ((ts-field (make-field 'timestamp :data-key :data)))

      (ok (typep (deserialize ts-field "2006-06-06TZ") 'local-time:timestamp)
	  "converts values to timestamps from LOCAL-TIME.")

      (ok (signals (deserialize ts-field "pizza")
	      'conversion-error)
	  "throws a conversion error on badly formatted timestamp."))))
