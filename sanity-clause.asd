(defsystem sanity-clause
  :author "Matt Novenstern"
  :license "LGPLv3"
  :version "0.7.4"
  :homepage "https://fisxoj.github.io/sanity-clause/"
  :depends-on ("alexandria"
               "arrows"
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
  :description "Sanity clause is a data contract and validation library."
  :long-description #.(uiop:read-file-string #P"README.rst")
  :in-order-to ((test-op (test-op sanity-clause/test))))


(defsystem sanity-clause/test
  :depends-on ("sanity-clause"
	       "rove")
  :pathname "t"
  :components ((:file "rove-junit-reporter")
               (:file "util")
	       (:file "field")
               (:file "loadable-schema")
               (:file "schema"))
  :perform (test-op (op c)
                    (declare (ignore op))
		    (uiop:symbol-call :rove/reporter/junit :run c
			     :env '(("VALUE" . "2")
				    ("POTATO" . "YAM")
				    ("AGE" . "11")
                                    ("FAVORITE_DOG" . "WEDGE")))))
