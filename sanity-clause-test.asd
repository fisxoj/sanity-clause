(defsystem sanity-clause-test
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("sanity-clause"
	       "prove")
  :pathname "t"
  :components ((:test-file "field")
	       (:test-file "sanity-clause"))
  :perform (test-op (op c)
		    (funcall (read-from-string "prove:run")
			     (system-relative-pathname :sanity-clause-test #P"t/"))))
