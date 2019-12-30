(defpackage sanity-clause/test.loadable-schema
  (:use #:cl
	#:rove
        #:alexandria))

(in-package :sanity-clause/test.loadable-schema)


(deftest test-load-schema
  (macrolet ((test-case (&key test-data (data-key-transformer '#'identity))
               `(let* ((schema (sanity-clause.loadable-schema:load-schema
                                (asdf:system-relative-pathname :sanity-clause/test "t/fixtures/schema.sexp")
                                :data-key-transformer ,data-key-transformer))
             (raw-data ',test-data)
             (data (sanity-clause.protocol:load schema raw-data)))

        (ok (typep (getf data :port) 'integer)
            "port is an integer.")

        (ok (typep (getf data :name) 'string)
            "name is a string.")

        (ok (eq (getf data :mode) :on)
            "member was converted to a keyword.")

        (ok (eq (getf data :boolean) nil)
            "boolean is off."))))

    (testing "can load a schema with keyword keys"
      (test-case :test-data ((:port .  3) (:name . "something") (:mode . "on") (:boolean . "no"))))

    (testing "can load a schema with string keys"
      (test-case :test-data (("port" . 3)
                             ("name" . "something")
                             ("mode" . "on")
                             ("boolean" . "no"))
                 :data-key-transformer #'string-downcase))))
