(defpackage sanity-clause.serde
  (:use #:cl
        #:alexandria
	#:cl-arrows)
  (:shadow #:load)
  (:export #:load))

(in-package #:sanity-clause.serde)

(defvar *serializers* nil
  "Plist of known serializers.")

(defgeneric load (schema data &optional format)
  (:documentation "Deserializes :param:`data` into an instance of :param:`class`.  Fills in default values and validates fields.")

  ;; Helper so you can (load 'some-class data) just like you can (make-instance 'some-class ...)
  (:method ((class t) data &optional format)
    (load (find-class class) data format))

  (:method ((class standard-class) (data list) &optional format)
    (->>
     (loop
       for field in (c2mop:class-slots class)
       for value = (sanity-clause.field:get-value field data (string-downcase (c2cl:slot-definition-name class)))

       do (sanity-clause.field:validate field value)

       when (sanity-clause.field:load-field-p field)
	 appending (list (first (c2mop:slot-definition-initargs field)) value))
     (apply #'make-instance class)))

  (:method ((schema list) (data list) &optional format)
    (loop
      for (marker field) on schema by #'cddr
      for value = (sanity-clause.field:get-value field data marker)

      do (sanity-clause.field:validate field value)

      when (sanity-clause.field:load-field-p field)
	appending (list marker value))))

(defgeneric serialize (field serializer value))

(defgeneric deserialize (field serializer value))

(defmacro defserializer (format &key
				  (name-to-lisp #'identity)
				  (lisp-to-name #'identity)))
