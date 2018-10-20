(defsystem sanity-clause
  :author "Matt Novenstern"
  :license "LLGPLv3+"
  :depends-on ("alexandria"
	       "trivial-types"
               "cl-arrows"
	       "validate"
	       "closer-mop"
	       "jonathan")
  :pathname "src"
  :components ((:file "util")
	       (:file "field")
	       (:file "schema")
	       (:module "serde"
		:components ((:file "protocol")
			     (:file "json")))
	       (:file "sanity-clause"))
  :in-order-to ((test-op (test-op sanity-clause-test))))
