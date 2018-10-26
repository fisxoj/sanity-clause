(defpackage sanity-clause.serde
  (:use #:cl
        #:alexandria
	#:cl-arrows)
  (:shadow #:load)
  (:export #:load))

(in-package #:sanity-clause.serde)

(defgeneric load (schema data &optional format)
  (:documentation "Deserializes :param:`data` into an instance of :param:`class`.  Fills in default values and validates fields.")

  ;; Helper so you can (load 'some-counclass data) just like you can (make-instance 'some-class ...)
  (:method ((class t) data &optional format)
    (load (find-class class) data format))

  (:method ((class standard-class) (data list) &optional format)
    (->>
     (loop
       for field in (c2mop:class-slots class)
       for value = (->>
		    (sanity-clause.field:get-value field data (string-downcase (c2cl:slot-definition-name class)))
		    (sanity-clause.field:deserialize field))

       do (sanity-clause.field:validate field value)

       when (sanity-clause.field:load-field-p field)
	 appending (list (first (c2mop:slot-definition-initargs field)) value))
     (apply #'make-instance class)))

  (:method ((schema list) (data list) &optional format)
    (loop
      for (marker field) on schema by #'cddr
      for value = (->> (sanity-clause.field:get-value field data marker)
		     (sanity-clause.field:deserialize field))

      do (sanity-clause.field:validate field value)

      when (sanity-clause.field:load-field-p field)
	appending (list marker value))))


(defgeneric dump (schema data &optional format)
  (:method ((schema symbol) data &optional (format :alist))
    (dump (find-class schema) data format))

  (:method ((schema standard-class) (data list) &optional format)
    (loop
      for (marker field) on schema by #'cddr
      for value = (getf data marker)
      for field-name = (or (sanity-clause.field:data-key-of field)
                          (string marker))

      when (sanity-clause.field:dump-field-p field)
        if (eq format :alist)
          collect (cons field-name value)
        else
          append (list field-name value))))
