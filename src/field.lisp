(defpackage sanity-clause.field
  (:use #:cl
	#:alexandria
	#:cl-arrows)
  (:export #:field
           #:validation-error

	   #:load-field-p
	   #:dump-field-p

	   #:make-field

	   ;; Fields
	   #:string-field
	   #:email-field
	   #:member-field
	   #:integer-field
	   #:real-field
	   #:constant-field
	   #:timestamp-field
           #:boolean-field
	   #:uuid-field

	   ;; Readers
	   #:attribute-of
           #:data-key-of

	   ;; Methods
	   #:deserialize
	   #:validate
	   #:get-value

	   ;; Errors
	   #:validation-error
	   #:required-value-error
	   #:conversion-error
	   #:error-messages-of
	   )
  (:documentation "Field classes that can be used to validate data.

Also contains :function:`get-value`, :function:`deserialize`, and :function:`validate`, which represent the lifecycle of loading data from some other object."))

(in-package :sanity-clause.field)


(deftype missing ()
  :missing)


(defclass field ()
  ((attribute :type (or null symbol)
	      :initarg :attribute
	      :initform nil
	      :reader attribute-of
	      :documentation "Name of the attribute to write the field's value to when serializing, if null, inferred from the name of the field.")
   (data-key :type (or null string symbol)
             :initform nil
	     :initarg :data-key
	     :reader data-key-of
	     :documentation "Name of the attribute to read the field's value from when deserializing, if null, inferred form the name of the field.")
   (default :type t
	    :initarg :default
	    :reader default-of
	    :initform :missing
	    :documentation "Value to use during serialization when no value is set.")
   (validator :type (or symbol function list)
	      :initarg :validator
	      :initform (constantly nil)
	      :reader validator-of)
   (data-flow :type (member :both :load :dump)
	      :initarg :flow
	      :initform :both
	      :reader data-flow-of
	      :documentation "If data should only ever be loaded into this field, this is :both (the default).  If data should only be deserialized from the field and ignored when serializing, :load.  If data should only be serialized from the field but ignored during loading, :dump.")
   (required :type boolean
	     :initarg :required
	     :initform nil
	     :reader required-p
	     :documentation "Is this field required?  Casuse the field to fail validation if it's not filled."))
  (:documentation "A base class for all fields that controls how they are (de?)serialized."))


(defun load-field-p (field)
  "Is this field required for deserialization (fields that have flow of :load or :both)?"

  (-> field
      data-flow-of
      (member '(:both :load))))


(defun dump-field-p (field)
  "Is this field required for serialization (fields that have flow of :dump or :both)?"

  (-> field
      data-flow-of
      (member '(:both :dump))))


(defun make-field (type &rest args)
  "Make a field instance of class ``type-FIELD`` and give it initargs :param:`args`."

  (if-let ((class (find-class (find-symbol (concatenate 'string (string-upcase type) "-FIELD") (find-package :sanity-clause.field)))))
    (apply #'make-instance class args)
    (error (format nil "No field class named ~@(~A~)-FIELD" type))))


(defclass string-field (field)
  ()
  (:documentation "A field that contains a string."))


(defclass member-field (field)
  ((members :initarg :members
            :reader members-of
            :initform (error "A member field requires a list of symbols that are acceptable members.")))
  (:documentation "A field that expects a member of a set of symbols."))


(defclass list-field (field)
  ((element-field :type field
                  :initarg :element-field
                  :reader element-field-of
                  :documentation "The field that respresents the elements of the list."))
  (:documentation "A field that contains a list of values satsified by another field."))


(defclass nested-field (field)
  ((nested-field :type field
                 :initarg :schema
		 :reader schema-of
		 :initform (error "A nested field requires a nested field instance.")
		 :documentation "A field that represents a complex object located at this field.")))


(defclass boolean-field (field)
  ()
  (:documentation "A field type for bolean values."))


(defclass email-field (string-field)
  ((validator :initform 'sanity-clause.validator:email))
  (:documentation "A field for values that should be emails."))


(defclass uuid-field (string-field)
  ((validator :initform 'sanity-clause.validator:uuid))
  (:documentation "A field for values that should resemble UUIDs."))


(defclass constant-field (field)
  ((constant :initarg :constant
             :reader constant-value-of
             :documentation "The constant value to be serialized or deserialized.")
   (test :initarg :test
	 :initform 'equal
	 :reader constant-test-of
	 :documentation "Function to use to compare the constant value to other values."))
  (:documentation "A field that expects to get the same value every time.  Will throw a :class:`conversion-error` if VALUE isn't equal to CONSTANT according to TEST."))


(defclass integer-field (field)
  ((validator :initform 'sanity-clause.validator:int))
  (:documentation "A field that holds an integer value."))


(defclass real-field (field)
  ()
  (:documentation "A field that contains a real value (eg. possibly a float)."))


(defclass timestamp-field (field)
  ()
  (:documentation "A field that contains a timestamp"))


(define-condition field-error (error)
  ((message :type string
            :initarg :message
            :reader message-of)
   (field :type field
	  :initarg :field
	  :reader field-of)
   (value :type t
	  :initarg :value
	  :reader value-of
	  :documentation "The value that failed validation."))
  (:documentation "Base class for all errors thrown by :package:`sanity-clause.field`."))


(define-condition validation-error (field-error)
  ((error-messages :type list
		   :initarg :error-messages
		   :initform nil
		   :reader error-messages-of
		   :documentation "A list of any error messages generated by the a field."))
  (:documentation "Error that indicates a field is invalid."))


(defmethod print-object ((condition validation-error) stream)
  (format stream "~& Error validating value ~A in field ~A:~%~{* ~a~}~%"
	  (value-of condition)
	  (field-of condition)
	  (error-messages-of condition)))


(define-condition conversion-error (field-error)
  ((raised-error :initarg :from-error
		 :reader raised-error-of
		 :documentation "The error that was caught while converting."))
  (:documentation "An error that signals something went wrong while calling ``converter`` for a field."))


(defmethod print-object ((condition conversion-error) stream)
  (format stream "~& Error converting value for field ~A: ~%~A~%"
	  (field-of condition)
	  (raised-error-of condition)))


(define-condition required-value-error (field-error)
  ((missing-field-name :type (or symbol string)
		       :initarg :field-name
		       :reader missing-field-name-of
		       :documentation "The name of the field that is missinga required value."))
  (:documentation "An error that signals a required value is missing."))


(defun all-validators (field)
  "Returns a generator function that yields a validator function each call."
  (declare (type field field))

  (etypecase (validator-of field)
    (null (list (constantly nil)))
    (symbol (list (validator-of field)))
    (function (list (validator-of field)))
    (list (validator-of field))))


(defmacro map-error (into-error-class &body body)
  `(handler-case (progn ,@body)
     (error (e)
       (error ',into-error-class :from-error e
				 :field field
				 :value value))))


(defgeneric deserialize (field value)
  (:documentation "Converts the value retrieved from the raw data into the datatype the field expects to work with, or fails, raising a :class:`conversion-error`.")

  (:method ((field field) value)
    value)

  ;; This also should cover some child fields like email
  (:method ((field string-field) value)
    (map-error conversion-error
      (etypecase value
	(string value))))

  (:method ((field integer-field) value)
    (map-error conversion-error
      (etypecase value
	(integer value)
	(string (parse-integer value)))))

  (:method ((field real-field) value)
    (map-error conversion-error
      (etypecase value
	(real value)
	(string (parse-float:parse-float value)))))

  (:method ((field member-field) value)
    (map-error conversion-error
      (etypecase value
	((or string symbol)
	 (if-let ((member (find value (members-of field) :test #'string-equal)))
	   member
	   (error 'conversion-error :message (format nil "Value \"~a\" couldn't be found in set ~a"
						     value
						     (members-of field))))))))

  (:method ((field boolean-field) value)
    (map-error conversion-error
      (etypecase value
	(string (cond
		  ((member value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string-equal) t)
		  ((member value '("n" "no"  "f" "false" "off" "disable") :test #'string-equal) nil)
		  (t (error 'conversion-error "couldn't convert ~a to a boolean." value))))
	(boolean value))))

  (:method ((field timestamp-field) value)
    (map-error conversion-error
      (local-time:parse-timestring value))))


(defgeneric serialize (field value)
  (:documentation "Converts the value of a field into another representation.")

  (:method ((field field) value)
    value))


(defgeneric validate (field value)
  (:documentation "Run the validation checks for a given field and raise a :class:`sanity-clause.field:validation-error` if it is invalid.")

  (:method ((field field) value)
    (when-let ((errors (->> (loop
			      for validator in (all-validators field)

			      collecting (funcall validator value))
			    (remove-if #'null))))
      (error 'validation-error :error-messages errors
			       :field field
			       :value value)))

  (:method ((field constant-field) value)
    (map-error validation-error
      (unless (funcall (constant-test-of field) (constant-value-of field) value)
	(error 'validation-error :error-messages (list (format nil "Value ~a is not the constant value ~a" value (constant-value-of field)))
				 :field field
				 :value value)))))


(defmacro with-value-and-found-p (&body body)
  `(let ((field-marker (or (data-key-of field) field-name)))
     (multiple-value-bind (value found-p)
	 (sanity-clause.util:get-value object field-marker (default-of field))
       ,@body)))


(defgeneric get-value (field object &optional field-name)
  (:documentation "Tries to fetch the value corresponding to the field from some datastructure.  :param:`field-name` is used if no attribute is explicitly set on the field.")

  (:method ((field field) object &optional field-name)
    (with-value-and-found-p
      (if (and (required-p field) (not found-p))
          (error 'required-value-error :field-name field-marker)
          value)))

  (:method ((field constant-field) object &optional field-name)
    (with-value-and-found-p
      (cond
        ((and (required-p field) (not found-p))
         (error 'required-value-error :field-name field-marker))
        ((not found-p) (constant-value-of field))
        (t value)))))
