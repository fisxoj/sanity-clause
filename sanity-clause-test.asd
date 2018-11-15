(defsystem sanity-clause-test
  :depends-on ("sanity-clause"
	       "rove")
  :pathname "t"
  :components ((:file "util")
	       (:file "field")
	       (:file "sanity-clause")
	       (:file "serde/protocol"))
  :perform (test-op (op c)
		    (funcall (read-from-string "rove:run") c
			     :env '(("VALUE" . "2")
				    ("POTATO" . "YAM")
				    ("AGE" . "11")))))
