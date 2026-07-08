(defsystem sanity-clause-2
  :pathname "src/2/"
  :depends-on ("str"
               "trivia")
  :version "0.0.1"
  :components ((:file "package")
               (:module "serde"
                :components ((:file "package")
                             (:file "environment")))))


(defsystem sanity-clause-2/test
  :depends-on ("sanity-clause-2"
               "parachute"))
