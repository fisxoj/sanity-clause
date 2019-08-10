(defpackage sanity-clause.loadable-schema
  (:use #:cl #:alexandria)
  (:documentation "Loadable schemas can be expressed as plists of keywords and then can be loaded by :function:`load`, either from a file, or a list.

You could, for example, define the configuration of a program that reads from the environment with::

  (setq schema (sanity-clause.loadable-schema:load #P\"my-schema.sexp\"))

And then you could load it from the environment with::

  (sanity-clause.schema:load schema :env)

your ``my-schema.sexp`` might look like::

  (:name (:string :validator (:not-empty) :default \"lisa\" :required t)
   :age (:integer :validator ((:int :min 0)) :required t))")
  (:export #:load-schema))

(in-package :sanity-clause.loadable-schema)


(defun load-schema (schema)
  "Takes a :type:`pathname` or schema spec list like::

  (:key (:string :validator (:not-empty) :default \"potato\")
   :key2 (:integer :validator ((:int :min 0)) :default 2))

and returns a schema plist with fields."

  (typecase schema
    (pathname (load (uiop:with-safe-io-syntax () (uiop:read-file-form schema))))
    (cons (loop for (key (type . spec)) on schema by #'cddr
                appending (list key (apply #'sanity-clause.field:make-field type (append (list :data-key key) (sanity-clause.validator:hydrate-validators spec))))))))
