(defpackage t.sanity-clause.serde.json
  (:use #:cl
	#:prove
	#:alexandria
	#:sanity-clause.serde.json))

(in-package #:t.sanity-clause.serde.json)

(plan 2)

(subtest "lisp-name-to-json-name"
  (let ((cases '(("something" . "something")
		 ("potato-type" . "potatoType")
		 ("some-cool-thing" . "someCoolThing"))))
    (loop for (lisp . json) in cases
	  do (is (lisp-name-to-json-name lisp) json))))

(subtest "json-name-to-lisp-name"
  (let ((cases '(("something" . "something")
		 ("potatoType" . "potato-type")
		 ("someCoolThing" . "some-cool-thing"))))
    (loop for (json . lisp) in cases
	  do (is (json-name-to-lisp-name json) lisp))))

(finalize)
