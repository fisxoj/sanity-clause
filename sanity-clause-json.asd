(defsystem sanity-clause-json
  :depends-on ("sanity-clause"
	       "jonathan")
  :pathname "src/serde/"
  :components ((:file "json")))
