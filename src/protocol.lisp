(defpackage sanity-clause.protocol
  (:use :cl)
  (:shadow #:load)
  (:export #:resolve
	   #:deserialize
	   #:serialize
	   #:validate
	   #:get-value
           #:load
           #:dump)
  (:documentation "The methods that govern behavior in sanity-clause.

The methods relating to fields are:

* :function:`resolve`
* :function:`deserialize`
* :function:`serialize`
* :function:`validate`
* :function:`get-value`

The methods relating to schemas are:

* :function:`load`
* :function:`dump`"))

(in-package :sanity-clause.protocol)

(defgeneric resolve (field data &optional parents)
  (:documentation "A function that encapsulates getting, corecing, and validating values for a field.  Calls :function:`get-value`, :function:`deserialize`, and :function:`validate`."))


(defgeneric deserialize (field value)
  (:documentation "Converts the value retrieved from the raw data into the datatype the field expects to work with, or fails, raising a :class:`sanity-clause.field:conversion-error`."))


(defgeneric serialize (field value)
  (:documentation "Converts the value of a field into another representation."))


(defgeneric validate (field value)
  (:documentation "Run the validation checks for a given field and raise a :class:`sanity-clause.field:validation-error` if it is invalid."))


(defgeneric get-value (field object)
  (:documentation "Tries to fetch the value corresponding to the field from some datastructure.  :param:`field-name` is used if no attribute is explicitly set on the field."))


(defgeneric load (schema data &optional format)
  (:documentation "Deserializes :param:`data` into an instance of :param:`schema`.  Fills in default values and validates fields.  If :param:`schema` is ``:env``, it will try to load from environment variables."))


(defgeneric dump (schema data &optional format)
  (:documentation "WARNING: This isn't fully tested/supported, but it might work!  I reserve the right to change the API in breaking ways, though.

dump takes some data an serializes it to a plist or alist based on :param:`format`, which can be set to, well, ``:plist`` or ``:alist``."))
