(defsystem sanity-clause
  :author "Matt Novenstern"
  :license "LLGPLv3+"
  :version "0.5.1"
  :homepage "https://fisxoj.github.io/sanity-clause/"
  :depends-on ("alexandria"
               "cl-arrows"
               "cl-ppcre"
               "closer-mop"
               "local-time"
	       "str"
	       "trivial-types"
	       "parse-float"
               "quri")
  :pathname "src"
  :components ((:file "util")
               (:file "validator")
               (:file "protocol")
	       (:file "field")
               (:file "loadable-schema")
               (:file "schema")
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
               (:file "schema"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove :run c
			     :env '(("VALUE" . "2")
				    ("POTATO" . "YAM")
				    ("AGE" . "11")
                                    ("FAVORITE_DOG" . "WEDGE")))))
