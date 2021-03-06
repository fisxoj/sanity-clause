(defpackage sanity-clause/test.field
  (:use #:cl
	#:rove
	#:sanity-clause.field
        #:alexandria
        #:arrows))

(in-package #:sanity-clause/test.field)

(eval-when (:load-toplevel :compile-toplevel)
  (defvar +manual-only-fields+ '(map-field list-field nested-field one-field-of-field one-schema-of-field member-field)
    "Symbols of field types that require extra data to exist, like :class:`member-field`, which requires a set of symbols as an initarg."))

;; (setf +manual-only-fields+ '(map-field list-field nested-field one-field-of-field one-schema-of-field member-field))

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
      "can find a field by string.")

  (ok (signals (sanity-clause.field:find-field nil))
      "raises an error when it can't find the given field."))

(deftest test-get-value
  (testing "missing values"
    (let ((default-field (make-instance 'field :default 3 :attribute 'potato-count))
	  (field (make-instance 'field :attribute 'potato-count))
	  (clos-inventory (make-instance 'inventory))
	  (plist-inventory '())
	  (alist-inventory))

      (ok (= (sanity-clause.protocol:get-value default-field clos-inventory) 3)
	  "Default values work for clos objects.")

      (ok (eq (sanity-clause.protocol:get-value field clos-inventory) :missing)
	  "A missing value with no default is :missing for clos objects.")

      (ok (= (sanity-clause.protocol:get-value default-field plist-inventory) 3)
	  "Default values work for plists.")

      (ok (eq (sanity-clause.protocol:get-value field plist-inventory) :missing)
	  "A missing value with no default is :missing for plists.")

      (ok (= (sanity-clause.protocol:get-value default-field alist-inventory) 3)
	  "Default values work for alists.")

      (ok (eq (sanity-clause.protocol:get-value field alist-inventory) :missing)
	  "A missing value with no default is :missing for alists."))))


(deftest test-deserialize
  (with-test-fields ()
    (testing "String field"
      (ok (sanity-clause.protocol:deserialize string-field "potato")
	  "deserializes a string.")

      (ok (signals (sanity-clause.protocol:deserialize string-field 4.5) 'conversion-error)
	  "signals an error when given a number."))

    (testing "Integer field"
      (ok (sanity-clause.protocol:deserialize integer-field "4")
	  "deserializes a string.")

      (ok (sanity-clause.protocol:deserialize integer-field 4)
	  "deserializes an integer.")

      (ok (signals (sanity-clause.protocol:deserialize integer-field 4.5) 'conversion-error)
	  "signals an error when given a float.")

      (ok (signals (sanity-clause.protocol:deserialize integer-field "garbage") 'conversion-error)
	  "signals an error when given a garbage string."))

    (testing "Real field"
      (ok (sanity-clause.protocol:deserialize real-field "3.1415")
	  "deserializes PI as a string ;).")

      (ok (sanity-clause.protocol:deserialize real-field 3.1415)
	  "deserializes PI as a float ;).")

      (ok (signals (sanity-clause.protocol:deserialize real-field "garbage") 'conversion-error)
	  "signals an error when given a garbage string.")

      (ok (signals (sanity-clause.protocol:deserialize real-field :garbage) 'conversion-error)
	  "signals an error when given a non-string and non-real datatype."))

    (testing "Member field"
      (let ((member-field (make-field 'member :members '(:yam :yukon :idaho))))
	(ok (sanity-clause.protocol:deserialize member-field "YaM")
	    "deserializes a string with different case into its union member.")

	(ok (sanity-clause.protocol:deserialize member-field :idaho)
	    "deserializes a keyword that is a union member.")

	(ok (signals (sanity-clause.protocol:deserialize member-field :woodear) 'conversion-error)
	    "signals an error for a keyword that isn't a member of the union.")

	(ok (signals (sanity-clause.protocol:deserialize member-field "washington") 'conversion-error)
	    "signals an error for a string that isn't a member of the union.")

	(ok (signals (sanity-clause.protocol:deserialize member-field 3) 'conversion-error)
	    "signals an error for a non-string-like type.")))

    (testing "URI field"
      (ok (sanity-clause.protocol:deserialize uri-field "https://something.com/potato?woo=bar#id")
          "accpets a valid uri")

      (skip "Not sure how to validate uris, yet"
       ;; (ok (signals (sanity-clause.protocol:deserialize uri-field "woo//bugs") 'conversion-error)
       ;;     "signals an error on a non-uri value.")
       ))


    (testing "Boolean field"
      (ok (eq (sanity-clause.protocol:deserialize boolean-field "on") t)
          "converts a truthy string to t.")

      (ok (signals (sanity-clause.protocol:deserialize boolean-field "wumbo") 'conversion-error)
          "signals an error for an uncertain value."))

    (testing "Timestamp field"
      (ok (typep (sanity-clause.protocol:deserialize timestamp-field "2006-06-06TZ") 'local-time:timestamp)
	  "converts values to timestamps from LOCAL-TIME.")

      (ok (signals (sanity-clause.protocol:deserialize timestamp-field "pizza") 'conversion-error)
	  "throws a conversion error on badly formatted timestamp."))))


(deftest test-validate
  (testing "validation-error"
    ;; Two disjoint criteria to generate two validation errors
    (let ((test-field (make-field 'integer :validator (list (lambda (v) (sanity-clause.validator:int v :max -3))
							    (lambda (v) (sanity-clause.validator:int v :min 3))))))
      (handler-case
	  (progn (sanity-clause.protocol:validate test-field "woo.com")
		 (fail "invalid data didn't raise a validation-error."))

	(validation-error (e)
	  (ok (= (length (error-messages-of e)) 2)
	      "had more than 0 error messages.")))))

  (with-test-fields ()
    (testing "Integer field"
      (ok (eq (sanity-clause.protocol:validate integer-field 4) nil)
	  "validates an integer."))

    (testing "Email field"
      (ok (null (sanity-clause.protocol:validate email-field "yam@potato.spud"))
	  "validates an email address.")

      (ok (signals (sanity-clause.protocol:validate email-field "wooo.com") 'validation-error)
	  "raises an error when given an invalid email address."))

    (testing "String field"
      (ok (null (sanity-clause.protocol:validate string-field "some string"))
	  "accepts any string."))

    (testing "Constant field"
      (let ((constant-string-field (make-field 'constant :constant "potato" :test 'string=))
	    (constant-number-field (make-field 'constant :constant 3 :test '=))
	    (constant-keyword-field (make-field 'constant :data-key 'potato-type :constant :russet)))

	(ok (null (sanity-clause.protocol:validate constant-string-field "potato"))
	    "validates a correct string with string=.")

	(ok (signals (sanity-clause.protocol:validate constant-string-field "worm") 'validation-error)
	    "raises an error for a value that isn't the constant according to string=.")

	(ok (null (sanity-clause.protocol:validate constant-number-field 3))
	    "validates a constant number value with =.")

	(ok (signals (sanity-clause.protocol:validate constant-number-field 5) 'validation-error)
	    "raises an error for a value that isn't = to a constant.")

	(ok (signals (sanity-clause.protocol:validate constant-keyword-field :armadillo) 'sanity-clause.field:validation-error)
	    "raises an error if the value doesn't match the constant value")))

    (testing "UUID field"
      (ok (null (sanity-clause.protocol:validate uuid-field "74827715-C657-4122-B6CF-63E3FA700FF6"))
	  "accepts a uuid string.")

      (ok (signals (sanity-clause.protocol:validate uuid-field "74827715-GGGG-4122-B6CF-63E3FA700FF6") 'validation-error)
	  "raises validation errors for something that looks like a uuid, but has forbidden 'G's in it."))))


(deftest test-missing-values
  (defclass t-m-v-pizza ()
    ((type :field-type :member
           :initarg :type
           :members (:pepperoni :hawaiian :plain)
           :required t)
     (size :field-type :member
           :initarg :size
           :members (:small :medium :large))
     (rating :field-type :integer
             :initarg :rating
             :validator (lambda (rating) (sanity-clause.validator:int rating :min 0 :max 5))))
    (:metaclass sanity-clause:validated-metaclass))

  (ok (signals (sanity-clause:load 't-m-v-pizza
                                   '(:size :small))
          'required-value-error)
      "a required field signals an error when it's not supplied.")


  (ok (not (slot-boundp (sanity-clause:load 't-m-v-pizza '(:type :plain)) 'rating))
      "non-required fields are left unbound if data isn't supplied."))


(deftest test-nested-field
  (defclass cat ()
    ((name :type string
           :data-key :name)
     (age :type integer
          :initarg :data-key))
    (:metaclass sanity-clause:validated-metaclass))

  (defclass human ()
    ((name :type string
           :initarg :name)
     (cat-friend :field-type :nested
                 :element-type cat
                 :initarg :cat-friend))
    (:metaclass sanity-clause:validated-metaclass))


  (defclass human-with-cat-list ()
    ((name :type string
           :data-key :name)
     (cat-friends :field-type :list
                  :element-type (:nested :element-type cat)
                  :data-key :cat-friends))
    (:metaclass sanity-clause:validated-metaclass))

  (testing "simple nesting"

    (let ((data-with-cat '(:name "Matt" :cat-friend (:name "Tara" :age 10)))
          (data-without-cat '(:name "Matt")))


      (ok (typep (slot-value (sanity-clause:load 'human data-with-cat) 'cat-friend) 'cat)
          "deserializes a cat class also, when data exists.")

      (ok (not (slot-boundp (sanity-clause:load 'human data-without-cat) 'cat-friend))
          "leaves a slot unbound, when data is missing and the slot isn't required.")))

  (testing "list nesting"
    (let ((data-with-cats '(:name "Matt"
                            :cat-friends ((:name "Tara"
                                           :age 10)
                                          (:name "Tiger"
                                           :age 4))))
          (data-without-cats-field '(:name "Matt"))
          (data-with-zero-cats '(:name "Matt" :cat-friends ())))


      (ok (= (length (slot-value (sanity-clause:load 'human-with-cat-list data-with-cats) 'cat-friends)) 2)
          "deserializes a list of cat classes, when data exists.")

      (ok (zerop (length (slot-value (sanity-clause:load 'human-with-cat-list data-with-zero-cats) 'cat-friends)))
          "deserializes a list of zero cat classes, when fields exists but is empty.")

      (ok (not (slot-boundp (sanity-clause:load 'human-with-cat-list data-without-cats-field) 'cat-friends))
          "leaves a slot unbound, when data is missing and the slot isn't required."))))


(deftest test-one-schema-of-field
  (testing "A one-schema-of-field with two options"

    (defclass tosof-pizza ()
      ((name :data-key :name
             :type string
             :required t))
      (:metaclass sanity-clause.schema:validated-metaclass))

    (defclass tosof-weasel-count ()
      ((count :data-key :count
              :type integer
              :validate (lambda (v) (sanity-clause.validator:int v :min 0))
              :required t))
      (:metaclass sanity-clause.schema:validated-metaclass))

    (let ((dumb-field (sanity-clause.field:make-field :one-schema-of
                                                      :schema-choices '(tosof-pizza tosof-weasel-count)
                                                      :required t)))

      (ok (typep (sanity-clause.protocol:deserialize dumb-field '(:name "pepperoni")) 'tosof-pizza)
          "decodes the first option.")

      (ok (typep (sanity-clause.protocol:deserialize dumb-field '(:count "112")) 'tosof-weasel-count)
          "decodes the second option.")

      (ok (signals (sanity-clause.protocol:deserialize dumb-field '(:armadillo :arnie)) 'sanity-clause.field:conversion-error)
          "signals an error if it can't decode either option.")))

  (testing "a descriminated union based on a 'version' field)"
    (defclass tosof-v1 ()
      ((version :field-type :constant
                :initarg :version
                :constant "1"
                :required t))
      (:metaclass sanity-clause:validated-metaclass))

    (defclass tosof-v2 ()
      ((version :field-type :constant
                :initarg :version
                :constant "2"
                :required t))
      (:metaclass sanity-clause:validated-metaclass))

    (defclass tosof-v3 ()
      ((version :field-type :constant
                :initarg :version
                :constant "3"
                :required t))
      (:metaclass sanity-clause:validated-metaclass))

    (let ((union-type-field (make-field :one-schema-of :schema-choices '(tosof-v1 tosof-v2 tosof-v3))))
      (ok (every 'eq
                 '(tosof-v3 tosof-v1 tosof-v2)
                 (mapcar (lambda (data) (class-name (class-of (sanity-clause.protocol:deserialize union-type-field data))))
                         '((:version "3") (:version "1") (:version "2"))))
          "decodes to the correct version of the class."))))


(deftest test-one-field-of-field
  (testing "a one-of-field defined with :field-choices class syntax"
    (let* ((string-field (make-field :string :validator 'sanity-clause.validator:not-empty))
           (integer-field (make-field :integer))
           (field (sanity-clause:make-field :one-field-of :data-key :data :field-choices (list string-field integer-field))))

      (ok (typep (sanity-clause.protocol:resolve field '(:data "hello")) 'string)
          "accepts a string.")

      (ok (typep (sanity-clause.protocol:resolve field '(:data 4)) 'integer)
          "accepts an integer.")

      (ok (signals (sanity-clause.protocol:resolve field (list :data (local-time:now))) 'sanity-clause.field:conversion-error)
          "signals an error for a datetime.")))

  (testing "a one-of-field defined with :field-choices keyword syntax"
    (let ((field (sanity-clause:make-field :one-field-of :data-key :data :field-choices '((:string :validator (:not-empty)) :integer))))

      (ok (typep (sanity-clause.protocol:resolve field '(:data "hello")) 'string)
          "accepts a string.")

      (ok (typep (sanity-clause.protocol:resolve field '(:data 4)) 'integer)
          "accepts an integer.")

      (ok (signals (sanity-clause.protocol:resolve field (list :data (local-time:now))) 'sanity-clause.field:conversion-error)
          "signals an error for a datetime."))))
