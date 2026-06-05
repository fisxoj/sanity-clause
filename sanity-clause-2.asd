(defsystem sanity-clause-2
  :pathname "src/2/"
  :depends-on ("trivia")
  :components ((:file "package")))


(defsystem sanity-clause-2/test
  :depends-on ("sanity-clause-2"
               "parachute"))
