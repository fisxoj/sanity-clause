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


(define-test constant-field
  (let ((field (put::make-field 'constant :constant :yam)))
    (parachute:finish (put::validate field :yam))
    (parachute:fail (put::validate field :idaho) 'put::validation-error))

  ;; A test function that can't handle the value's type is a validation
  ;; error, not a crash.
  (let ((field (put::make-field 'constant :constant "5" :test 'string=)))
    (parachute:fail (put::validate field 5) 'put::validation-error)))


(define-test uuid-field
  (let ((field (put::make-field 'uuid)))
    (parachute:finish (put::validate field "00cf3f24-b5ef-4f47-bd2c-e1cdc492fce5"))
    (parachute:finish (put::validate field "00CF3F24-B5EF-4F47-BD2C-E1CDC492FCE5"))
    (parachute:fail (put::validate field "00cf3f24b5ef4f47bd2ce1cdc492fce5") 'put::validation-error)
    (parachute:fail (put::validate field 4) 'put::validation-error)))


(define-test uri-field
  (let ((field (put::make-field 'uri)))
    (parachute:finish (put::validate field "https://example.com/potato"))
    (parachute:finish (put::validate field (quri:uri "https://example.com/potato")))
    (parachute:fail (put::validate field "::::") 'put::validation-error)
    (parachute:fail (put::validate field "http://exa mple.com/x") 'put::validation-error)
    (parachute:fail (put::validate field 4) 'put::validation-error)))


(define-test timestamp-field
  (let ((field (put::make-field 'timestamp)))
    (parachute:finish (put::validate field (local-time:now)))
    (parachute:fail (put::validate field "2026-04-25T10:00:00Z") 'put::validation-error)))


(define-test hash-table-field
  (let ((field (put::make-field 'hash-table :key-field 'string :value-field '(integer :minimum 1 :maximum 10))))
    (parachute:finish (put::validate field (alexandria:alist-hash-table '(("a" . 1) ("b" . 2)) :test 'equal)))
    (parachute:finish (put::validate field (alexandria:alist-hash-table nil :test 'equal)))
    (parachute:fail (put::validate field (alexandria:alist-hash-table '(("a" . 11)) :test 'equal)) 'put::hash-table-validation-error)
    (parachute:fail (put::validate field (alexandria:alist-hash-table '((1 . 2)) :test 'equal)) 'put::hash-table-validation-error)
    (parachute:fail (put::validate field '(("a" . 1))) 'put::validation-error))

  ;; Keys and values default to strings
  (let ((field (put::make-field 'hash-table)))
    (of-type put::string-field (put::key-field field))
    (of-type put::string-field (put::value-field field))
    (parachute:finish (put::validate field (alexandria:alist-hash-table '(("a" . "b")) :test 'equal)))))
