(defpackage sanity-clause
  (:use #:cl
	#:alexandria)
  (:documentation "Main package of the :package:`sanity-clause` system.

:package:`sanity-clause` helps define data contracts like \"this field will always have a valid integer greater than zero in it\".  It's also useful for serializing and deserializing data to and from different formats.  Contracts are built in the form of schemas, which are composed of :class:`sanity-clause.field:field` s."))

(in-package #:sanity-clause)
