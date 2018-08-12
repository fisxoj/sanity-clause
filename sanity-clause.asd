(defsystem sanity-clause
  :author "Matt Novenstern"
  :license "LLGPLv3+"
  :depends-on ("alexandria"
	       "trivial-types"
               "cl-arrows"
	       "validate")
  :pathname "src"
  :components ((:file "field")
	       (:file "sanity-clause"))
  :in-order-to ((test-op (test-op sanity-clause-test))))
