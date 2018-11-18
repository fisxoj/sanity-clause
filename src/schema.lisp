(defpackage sanity-clause.schema
  (:use #:cl #:alexandria)
  (:shadow #:load)

  (:export #:load))

(in-package :sanity-clause.schema)


(defun load (schema)
  "Takes a :type:`pathname` or schema spec list like::
  (:key (:string :validator (:not-empty) :default \"potato\")
   :key2 (:integer :validator ((:int :min 0)) :default 2))

and returns a schema plist with fields."

  (flet ((hydrate-validators (spec)
           (when-let ((validator-spec (getf spec :validator)))
             (setf (getf spec :validator) (mapcar #'sanity-clause.validator:make-validator validator-spec)))
           spec))

    (typecase schema
      (pathname (load (uiop:with-safe-io-syntax () (uiop:read-file-form schema))))
      (cons (loop for (key (type . spec)) on schema by #'cddr
                  appending (list key (apply #'sanity-clause.field:make-field type (hydrate-validators spec))))))))
