(defsystem sanity-clause-test
  :defsystem-depends-on ("prove-asdf")
  :depends-on ("sanity-clause"
	       "rove")
  :pathname "t"
  :components ((:file "util")
	       (:file "field")
	       (:file "schema")
	       (:file "sanity-clause")
	       (:file "serde/protocol")
	       (:file "serde/json"))
  :perform (test-op (op c)
		    (funcall (read-from-string "rove:run") c)))
