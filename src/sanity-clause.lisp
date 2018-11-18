(defpackage sanity-clause
  (:use #:cl
	#:alexandria)
  (:import-from #:sanity-clause.field
		#:make-field)
  (:shadowing-import-from #:sanity-clause.serde
			  #:load)
  (:export #:load
	   #:make-field)

  (:documentation "Main package of the :package:`sanity-clause` system.

:package:`sanity-clause` helps define data contracts like \"this field will always have a valid integer greater than zero in it\".  It's also useful for serializing and deserializing data to and from different formats.  Contracts are built in the form of schemas, which are composed of :class:`sanity-clause.field:field` s.

for example::

  (setq schema (list :name (make-field 'string)
                     :age  (make-field 'integer :validator (lambda (v) (unless (> v 0) \"impossible age!\")))))

  (setq some-data '((:name . \"matt\")
                    (:age  . \"7\")))

  (load schema some-data)
  ;; => (:name \"matt\" :age 7)
  ;; note: the string for age got converted by DESERIALIZE from a string to an integer.

To learn more about what fields exist, check out :package:`sanity-clause.field`.

To see more validation functions, check out :package:`sanity-clause.validator`.

To see how to load a schema from a file or keyword spec, check out :package:`sanity-clause.schema`."))

(in-package #:sanity-clause)
