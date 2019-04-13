(defsystem sanity-clause
  :author "Matt Novenstern"
  :license "LLGPLv3+"
  :version "0.3.0"
  :depends-on ("alexandria"
	       "trivial-types"
               "cl-arrows"
               "closer-mop"
	       "str"
	       "validate"
	       "parse-float"
	       "cl-ppcre")
  :pathname "src"
  :components ((:file "util")
               (:file "validator")
	       (:file "field")
               (:file "schema")
	       (:module "serde"
		:components ((:file "protocol")))
               (:module "metaclass"
                :components ((:file "types")
                             (:file "class")))
	       (:file "sanity-clause"))
  :long-description "Sanity clause is a data validation/contract library that can be used to collect and validate information.  You might use it for configuration data, or validating data from an api response, or documents from a datastore.

To make use of it, you define schemas, which are currently property lists with :class:`sanity-clause.field:field` subclasses that dictate the type of values you expect as well as the shape of the property list to be returned after deserializing and validating data.

Eventually, there will be an interface that allows creating schemas attached to classes, so that you can deserialize data directly into a class instance."
  :in-order-to ((test-op (test-op sanity-clause/test))))


(defsystem sanity-clause/test
  :depends-on ("sanity-clause"
	       "rove")
  :pathname "t"
  :components ((:file "util")
	       (:file "field")
               (:file "schema")
               (:module "metaclass"
                :components ((:file "types")
                             (:file "class")))
	       (:file "serde/protocol"))
  :perform (test-op (op c)
		    (funcall (read-from-string "rove:run") c
			     :env '(("VALUE" . "2")
				    ("POTATO" . "YAM")
				    ("AGE" . "11")
                                    ("FAVORITE_DOG" . "WEDGE")))))
