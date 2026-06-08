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
  :parent class-definition

  (of-type put::integer-field (put::infer-field-type '(integer 0 3)))
  (of-type put::integer-field (put::infer-field-type 'integer)))


(define-test validate-initargs
  (parachute:fail (make-instance 'dog :age -2)
      'put::validation-error))

(defclass widget ()
  ((price :type (or float string)
          :initarg :price))
  (:metaclass sanity-clause-2:metaclass))


(define-test or-field
  (parachute:finish (make-instance 'widget :price 3.0d0))

  (parachute:finish (make-instance 'widget :price "infinity"))

  (parachute:fail (make-instance 'widget :price t)
      'sanity-clause-2::or-validation-error))
