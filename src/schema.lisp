(defpackage sanity-clause.schema
  (:use #:cl
	#:alexandria))

(in-package #:sanity-clause.schema)

(defclass schema-class (standard-class) ())

(defclass validated-slot-definition (c2mop:standard-slot-definition)
  ((field :type sanity-clause.field::field)))

;; (defmethod c2mop:validate-superclass ((class schema-metaclass) (super standard-class))
;;   t)

(defmethod c2mop:direct-slot-definition-class ((class schema-class) &rest initargs)
  'sanity-clause.fields)
