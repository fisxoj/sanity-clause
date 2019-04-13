(defpackage sanity-clause.schema
  (:use #:cl
        #:alexandria
	#:cl-arrows)
  (:shadow #:load)
  (:export #:load
	   #:dump)

  (:documentation "This package defines the protocol for (de)serializing data to/from other datastructures."))

(in-package #:sanity-clause.schema)


(defgeneric load (schema data &optional format)
  (:documentation "Deserializes :param:`data` into an instance of :param:`schema`.  Fills in default values and validates fields.  If :param:`schema` is ``:env``, it will try to load from environment variables.")

  ;; Helper so you can (load 'some-counclass data) just like you can (make-instance 'some-class ...)
  ;; (:method ((class symbol) data &optional format)

  ;;   (load (find-class class) data format))

  ;; (:method ((class sanity-clause.metaclass:validated-metaclass) data &optional format)
  ;;   (declare (ignore format))

  ;;   (->>
  ;;    (let (initargs)
  ;;      (dolist (slot (c2mop:class-slots class))
  ;;        (let ((field (sanity-clause.metaclass.class::field-of slot)))

  ;;          (when (sanity-clause.field:load-field-p field)
  ;;            (let ((value (->>
  ;;                          (sanity-clause.field:get-value field data (string-downcase (c2mop:slot-definition-name class)))
  ;;                          (sanity-clause.field:deserialize field))))

  ;;              (sanity-clause.field:validate field value)

  ;;              (appendf initargs (list (first (c2mop:slot-definition-initargs field)) value)))))))
  ;;    (apply #'make-instance class)))

  (:method ((schema list) data &optional format)
    (declare (ignore format))

    (loop
      for (marker field) on schema by #'cddr
      for value = (->> (sanity-clause.field:get-value field data marker)
		       (sanity-clause.field:deserialize field))

      do (sanity-clause.field:validate field value)

      when (sanity-clause.field:load-field-p field)
	appending (list marker value))))


(defgeneric dump (schema data &optional format)
  (:documentation "WARNING: This isn't fully tested/supported, but it might work!  I reserve the right to change the API in breaking ways, though.

dump takes some data an serializes it to a plist or alist based on :param:`format`, which can be set to, well, ``:plist`` or ``:alist``.")

  (:method ((schema symbol) data &optional format)
    (dump (find-class schema) data format))

  (:method ((schema list) (data list) &optional format)
    (loop
      for (marker field) on schema by #'cddr
      for value = (getf data marker)
      for field-name = (or (sanity-clause.field:data-key-of field)
			   (string marker))

      when (sanity-clause.field:dump-field-p field)
	do (sanity-clause.field:validate field value)
	and if (eq format :alist)
          collect (cons field-name value)
        else
          append (list field-name value))))
