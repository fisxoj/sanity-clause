(defpackage t.sanity-clause.serde.json
  (:use #:cl
	#:rove
	#:alexandria
	#:sanity-clause.serde.json))

(in-package #:t.sanity-clause.serde.json)

(let ((cases '(("something" . "something")
	       ("potato-type" . "potatoType")
	       ("some-cool-thing" . "someCoolThing"))))

  (deftest test-lisp-name-to-json-name
    (loop for (lisp . json) in cases
	  do (ok (string= (lisp-name-to-json-name lisp) json)
		 (format nil "correctly converts ~S to ~S" lisp json))))

  (deftest test-json-name-to-lisp-name
    (loop for (lisp . json) in cases
	  do (ok (string= (json-name-to-lisp-name json) lisp)
		 (format nil "correctly converts ~S to ~S" json lisp)))))
