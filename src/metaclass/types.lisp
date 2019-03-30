(defpackage :sanity-clause.metaclass.types
  (:use :cl :alexandria)
  (:export #:slot-type-to-field-initargs))

(in-package :sanity-clause.metaclass.types)


(defun slot-type-to-field-initargs (typespec)
  (let ((typespec (ensure-list typespec)))
    (case (car typespec)
      (integer
       (values (find-class 'sanity-clause.field:integer-field)
               (list :validator (list (lambda (v) (sanity-clause.validator:int v :min (second typespec) :max (third typespec)))))))

      (string
       (values (find-class 'sanity-clause.field:string-field)
               (when-let ((length (second typespec)))
                 (list :validator (list (lambda (v) (sanity-clause.validator:str v :max-length length :min-length length)))))))

      (real (values (find-class 'sanity-clause.field:real-field) nil)))))
