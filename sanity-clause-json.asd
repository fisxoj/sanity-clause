(defsystem sanity-clause-json
  :depends-on ("sanity-clause"
	       "jonathan")
  :pathame "src/serde/"
  :components ((:file "json")))
