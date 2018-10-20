(defsystem sanity-clause-test
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("sanity-clause"
	       "prove")
  :pathname "t"
  :components ((:test-file "util")
	       (:test-file "field")
	       (:test-file "schema")
	       (:test-file "sanity-clause")
	       (:test-file "serde/protocol")
	       (:test-file "serde/json"))
  :perform (test-op (op c)
		    (funcall (read-from-string "prove:run")
			     (system-relative-pathname :sanity-clause-test #P"t/"))))
