(defsystem sanity-clause
  :author "Matt Novenstern"
  :license "LLGPLv3+"
  :version "0.2.0"
  :depends-on ("alexandria"
	       "trivial-types"
               "cl-arrows"
	       "str"
	       "validate"
	       "parse-float"
	       "cl-ppcre")
  :pathname "src"
  :components ((:file "util")
               (:file "validator")
	       (:file "field")
	       (:module "serde"
		:components ((:file "protocol")))
	       (:file "sanity-clause"))
  :long-description "Sanity clause is a data validation/contract library that can be used to collect and validate information.  You might use it for configuration data, or validating data from an api response, or documents from a datastore.

To make use of it, you define schemas, which are currently property lists with :class:`sanity-clause.field:field` subclasses that dictate the type of values you expect as well as the shape of the property list to be returned after deserializing and validating data.

Eventually, there will be an interface that allows creating schemas attached to classes, so that you can deserialize data directly into a class instance."
  :in-order-to ((test-op (test-op sanity-clause-test))))
