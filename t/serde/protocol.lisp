(defpackage t.sanity-clause.serde
  (:use #:cl
	#:rove
	#:alexandria
	#:sanity-clause.serde)
  (:import-from #:sanity-clause.field
		#:make-field
		#:conversion-error
		#:validation-error
		#:required-value-error)
  (:shadowing-import-from #:sanity-clause.serde
			  #:load))

(in-package #:t.sanity-clause.serde)


(deftest test-load
  (testing "plists"
    (let ((plist-schema (list :name (make-field 'string)
			      :age  (make-field 'integer :required t))))

      (ok (load plist-schema '(:name "Matt" :age 30))
	  "Can load from valid data.")

      (ok (load plist-schema '(:name "Matt" :age "30"))
	  "Can load valid data, converting from a string, if necessary.")

      (ok (signals (load plist-schema '(:name "Matt" :age "potato"))
              'conversion-error)
          "A string that isn't an int raises a conversion error.")

      (ok (signals (load plist-schema '(:name 11 :age 30))
              'conversion-error)
          "An integer that is not a string raises a validation error.")

      (ok (signals (load plist-schema '(:name "Matt"))
              'required-value-error)
          "A missing, required value raises a required value error."))))


(deftest test-examples
  (testing "sexp configuration file"
    (let* ((raw-configuration (uiop:with-safe-io-syntax ()  (uiop:read-file-form (asdf:system-relative-pathname :sanity-clause-test "t/fixtures/environment.sexp"))))
	   (schema (list :database-url (make-field 'string :validator (list 'sanity-clause.validator:not-empty
									    (lambda (v) (unless (str:starts-with-p "postgres" v :ignore-case t)
											  "expected a postgres protocol."))))
			 :port (make-field 'integer :validator (lambda (v) (sanity-clause.validator:int v :min 0)))
			 :mode (make-field 'member :members '(:development :testing :production))
			 :penguin (make-field 'boolean :default t)))
	   (configuration (load schema raw-configuration)))

      (ok (typep (getf configuration :database-url) 'string)
	  "database-uri is a valid string.")

      (ok (typep (getf configuration :port) 'integer)
	  "port was converted to an integer.")

      (ok (typep (getf configuration :mode) 'keyword)
	  "mode is a keyword.")

      (ok (typep (getf configuration :penguin) 'boolean)
	  "a default value for penguin is set."))))
