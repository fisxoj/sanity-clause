(defpackage :sanity-clause-2/test
  (:use #:cl
        #:parachute)
  (:local-nicknames (:put :sanity-clause-2)))

(in-package :sanity-clause-2/test)


(define-test class-definition
  (true (put:define-validated-class dog ()
          ((age :type (integer 0)
                :initarg :age))
          (:documentation "A dog")
          (:default-initargs :age 0))
        "Class definition runs without error"))

(define-test infer-field-type
  :serial nil

  (of-type put::integer-field (put::infer-field-type '(integer 0 3)))
  (of-type put::integer-field (put::infer-field-type 'integer)))


(define-test validate-initargs
  (parachute:fail (make-instance 'dog :age -2)
      'put::validation-error))
