(defsystem sanity-clause-2
  :pathname "src/2/"
  :depends-on ("str"
               "trivia")
  :components ((:file "package")
               (:module "serde"
                :components ((:file "package")
                             (:file "environment")))))


(defsystem sanity-clause-2/test
  :depends-on ("sanity-clause-2"
               "parachute"))
