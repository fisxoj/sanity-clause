(defpackage sanity-clause/test.loadable-schema
  (:use #:cl
	#:rove
        #:alexandria))

(in-package :sanity-clause/test.loadable-schema)


(deftest test-load-schema
  (let* ((schema (sanity-clause.loadable-schema:load (asdf:system-relative-pathname :sanity-clause/test "t/fixtures/schema.sexp")))
         (raw-data '(:port 3 :name "something" :mode "on" :boolean "no"))
         (data (sanity-clause.schema:load schema raw-data)))

    (ok (typep (getf data :port) 'integer)
        "port is an integer.")

    (ok (typep (getf data :name) 'string)
        "name is a string.")

    (ok (eq (getf data :mode) :on)
        "member was converted to a keyword.")

    (ok (eq (getf data :boolean) nil)
        "boolean is off.")))
