(defpackage sanity-clause/test.metaclass.types
  (:use :cl :alexandria :rove))

(in-package :sanity-clause/test.metaclass.types)


(deftest test-slot-type-to-field-initargs
  (ok (eq (sanity-clause.metaclass.types:slot-type-to-field-initargs '(integer 0 10)) (find-class 'sanity-clause.field:integer-field))
      "finds a field type for (INTEGER 0 10).")

  (ok (eq (sanity-clause.metaclass.types:slot-type-to-field-initargs 'integer) (find-class 'sanity-clause.field:integer-field))
      "finds a field type for INTEGER.")

  (ok (eq (sanity-clause.metaclass.types:slot-type-to-field-initargs '(string 10)) (find-class 'sanity-clause.field:string-field))
      "finds a field type for (STRING 10).")

  (ok (eq (sanity-clause.metaclass.types:slot-type-to-field-initargs 'string) (find-class 'sanity-clause.field:string-field))
      "finds a field type for STRING."))
