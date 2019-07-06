(defsystem sanity-clause
  :author "Matt Novenstern"
  :license "LLGPLv3+"
  :version "0.4.0"
  :homepage "https://fisxoj.github.io/sanity-clause/"
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
               (:file "loadable-schema")
               (:file "schema")
               (:file "metaclass")
	       (:file "sanity-clause"))
  :long-description #.(uiop:read-file-string #P"README.rst")
  :in-order-to ((test-op (test-op sanity-clause/test))))


(defsystem sanity-clause/test
  :depends-on ("sanity-clause"
	       "rove")
  :pathname "t"
  :components ((:file "util")
	       (:file "field")
               (:file "loadable-schema")
               (:file "schema")
               (:file "metaclass"))
  :perform (test-op (op c)
		    (funcall (read-from-string "rove:run") c
			     :env '(("VALUE" . "2")
				    ("POTATO" . "YAM")
				    ("AGE" . "11")
                                    ("FAVORITE_DOG" . "WEDGE")))))
