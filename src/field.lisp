(defpackage sanity-clause.field
  (:use #:cl
	#:alexandria
	#:cl-arrows)
  (:export #:field
           #:validation-error

	   #:load-field-p
	   #:dump-field-p

           #:find-field
	   #:make-field

	   ;; Fields
	   #:string-field
	   #:email-field
	   #:member-field
	   #:integer-field
	   #:real-field
	   #:constant-field
	   #:timestamp-field
           #:uri-field
           #:boolean-field
	   #:uuid-field

           #:map-field
           #:one-schema-of-field
           #:one-field-of-field

           ;; Fields defined in nested-fields.lisp
           ;; #:map-field
           ;; #:list-field
           ;; #:nested-field
           ;; #:one-field-of-field
           ;; #:one-schema-of-field

	   ;; Readers
	   #:attribute-of
           #:data-key-of

	   ;; Errors
	   #:validation-error
	   #:required-value-error
	   #:conversion-error
	   #:error-messages-of)
  (:documentation "Field classes that can be used to validate data.

Also contains :function:`sanity-clause.protocol:get-value`, :function:`sanity-clause.protocol:deserialize`, and :function:`sanity-clause.protocol:validate`, which represent the lifecycle of loading data from some other object."))

(in-package :sanity-clause.field)


(deftype missing ()
  '(eql :missing))


(defclass field ()
  ((attribute :type (or null symbol string)
	      :initarg :attribute
	      :initform nil
	      :reader attribute-of
	      :documentation "Name of the attribute to write the field's value to when serializing, if null, inferred from the name of the field.")
   (data-key :type (or null string symbol)
             :initform nil
	     :initarg :data-key
	     :accessor data-key-of
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
	     :documentation "Is this field required?  Cause the field to fail validation if it's not filled."))
  (:documentation "A base class for all fields that controls how they are (de?)serialized."))


(defmethod print-object ((object field) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@[data-key: ~a~]" (data-key-of object))))

;;; The most generic forms of these methods are defined here
(defun all-validators (field)
  "Returns a generator function that yields a validator function each call."
  (declare (type field field))

  (etypecase (validator-of field)
    (null (list (constantly nil)))
    (symbol (list (validator-of field)))
    (function (list (validator-of field)))
    (list (validator-of field))))


(defmethod sanity-clause.protocol:resolve :around ((field field) data &optional parents)
  (handler-case (call-next-method)
    ((or conversion-error validation-error) (e)
      ;; set parents and reraise
      (setf (parents-of e) parents)
      (error e))))


(defmethod sanity-clause.protocol:resolve ((field field) data &optional parents)
  (declare (ignore parents))

  (let ((value (sanity-clause.protocol:get-value field data)))

    (if (eq value :missing)
        value
        (let ((deserialized-value (sanity-clause.protocol:deserialize field value)))

          (sanity-clause.protocol:validate field deserialized-value)

          (values deserialized-value)))))


(defmethod sanity-clause.protocol:deserialize :around ((field field) value)
  ;; For some nicer error handling - wrap other methods and coerce thigns to conversion-error
  ;; so we know they failed here.
  (handler-case (call-next-method)
    (error (e)
       (error 'conversion-error :from-error e
                                :field field
                                :value value))))


(defmethod sanity-clause.protocol:deserialize ((field field) value)
  (declare (ignore field))

  value)


(defmethod sanity-clause.protocol:serialize ((field field) value)
  (declare (ignore field))

  value)


(defmethod sanity-clause.protocol:validate ((field field) value)
  (when-let ((errors (->> (loop
                            for validator in (all-validators field)

                            collecting (funcall validator value))
                          (remove-if #'null))))

    (error 'validation-error :error-messages errors
                             :field field
                             :value value)))


(defmethod sanity-clause.protocol:get-value ((field field) object)
  (multiple-value-bind (value found-p)
	 (sanity-clause.util:get-value object (data-key-of field) (default-of field))

    (if (and (required-p field) (not found-p))
        (error 'required-value-error :field-name (data-key-of field))
        value)))


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


(defun find-field (type)
  "Find the class that corresponds to :param:`type` by name"

  (if-let ((class (find-class (find-symbol (concatenate 'string (string-upcase type) "-FIELD")
                                           (find-package :sanity-clause.field)))))
    class
    (error "No field class named ~@(~A~)-FIELD" type)))


(defun make-field (type &rest args)
  "Make a field instance of class ``type-FIELD`` and give it initargs :param:`args`."

  (apply #'make-instance
         (etypecase type
           ((or symbol string) (find-field type))
           (class type))
         args))


(defmacro define-final-class (name direct-superclasses direct-slots &rest options)
  "A macro for definining classes that are finalized after definition."

  `(->
    (defclass ,name ,direct-superclasses
      ,direct-slots
      ,@options)
    c2mop:ensure-finalized))


;;; Everything below here is specific to a class of field

(define-final-class string-field (field)
  ()
  (:documentation "A field that contains a string."))


;; This also should cover some child fields like email
(defmethod sanity-clause.protocol:deserialize ((field string-field) value)
  (etypecase value
    (string value)))



(define-final-class member-field (field)
  ((members :initarg :members
            :reader members-of
            :initform (error "A member field requires a list of symbols that are acceptable members.")))
  (:documentation "A field that expects a member of a set of symbols."))


(defmethod sanity-clause.protocol:deserialize ((field member-field) value)
  (etypecase value
    ((or string symbol)
     (if-let ((member (find value (members-of field) :test #'string-equal)))
       member
       (error "Value \"~a\" couldn't be found in set ~a"
              value
              (members-of field))))))

(define-final-class boolean-field (field)
  ()
  (:documentation "A field type for bolean values."))


(defmethod sanity-clause.protocol:deserialize ((field boolean-field) value)
  (etypecase value
    (string (cond
              ((member value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string-equal) t)
              ((member value '("n" "no"  "f" "false" "off" "disable") :test #'string-equal) nil)
              (t (error "couldn't convert ~a to a boolean." value))))
    (boolean value)))


(define-final-class email-field (string-field)
  ((validator :initform 'sanity-clause.validator:email))
  (:documentation "A field for values that should be emails."))


(define-final-class uri-field (string-field)
  ()
  (:documentation "A field for values that should be emails."))


(defmethod sanity-clause.protocol:deserialize ((field uri-field) value)
  (etypecase value
    (string (quri:uri value))
    (quri.uri:uri value)))


(define-final-class uuid-field (string-field)
  ((validator :initform 'sanity-clause.validator:uuid))
  (:documentation "A field for values that should resemble UUIDs."))


(define-final-class constant-field (field)
  ((constant :initarg :constant
             :reader constant-value-of
             :documentation "The constant value to be serialized or deserialized.")
   (test :initarg :test
	 :initform 'equal
	 :reader constant-test-of
	 :documentation "Function to use to compare the constant value to other values."))
  (:documentation "A field that expects to get the same value every time.  Will throw a :class:`conversion-error` if VALUE isn't equal to CONSTANT according to TEST."))


(defmethod sanity-clause.protocol:validate ((field constant-field) value)
  (unless (funcall (constant-test-of field) (constant-value-of field) value)
    (error 'validation-error :error-messages (list (format nil "Value ~a is not the constant value ~a" value (constant-value-of field)))
                             :field field
                             :value value)))


(defmethod sanity-clause.protocol:get-value ((field constant-field) object)
  (multiple-value-bind (value found-p)
	 (sanity-clause.util:get-value object (data-key-of field) (default-of field))

    (cond
      ((and (required-p field) (not found-p))
       (error 'required-value-error :field-name (data-key-of field)))
      ((not found-p) (constant-value-of field))
      (t value))))


(define-final-class integer-field (field)
  ((validator :initform 'sanity-clause.validator:int))
  (:documentation "A field that holds an integer value."))


(defmethod sanity-clause.protocol:deserialize ((field integer-field) value)
  (etypecase value
    (integer value)
    (string (parse-integer value))))


(define-final-class real-field (field)
  ()
  (:documentation "A field that contains a real value (eg. possibly a float)."))



(defmethod sanity-clause.protocol:deserialize ((field real-field) value)
  (etypecase value
    (real value)
    (string (parse-float:parse-float value))))


(define-final-class timestamp-field (field)
  ()
  (:documentation "A field that contains a timestamp"))


(defmethod sanity-clause.protocol:deserialize ((field timestamp-field) value)
  (local-time:parse-timestring value))


(define-condition field-error (error)
  ((field :type field
	  :initarg :field
	  :reader field-of)
   (parents :accessor parents-of
           :initarg :parents))
  (:documentation "Base class for all errors thrown by :package:`sanity-clause.field`."))


(define-condition value-error (error)
  ((value :type t
          :initarg :value
          :reader value-of
          :documentation "The value that failed validation."))
  (:documentation "Base class for errors involving values."))


(define-condition validation-error (field-error value-error)
  ((error-messages :type list
		   :initarg :error-messages
		   :initform nil
		   :reader error-messages-of
		   :documentation "A list of any error messages generated by the a field."))
  (:documentation "Error that indicates a field is invalid."))


(defmethod print-object ((condition validation-error) stream)
  (format stream "Error validating value ~A in field ~A:~%~{* ~a~}~%"
	  (value-of condition)
	  (field-of condition)
	  (error-messages-of condition)))


(define-condition conversion-error (field-error value-error)
  ((raised-error :initarg :from-error
		 :reader raised-error-of
		 :documentation "The error that was caught while converting."))
  (:documentation "An error that signals something went wrong while calling ``converter`` for a field."))


(defmethod print-object ((condition conversion-error) stream)
  (format stream "Error converting value for field ~A: ~%~A~%"
	  (field-of condition)
	  (raised-error-of condition)))


(define-condition required-value-error (field-error)
  ((missing-field-name :type (or symbol string)
		       :initarg :field-name
		       :reader missing-field-name-of
		       :documentation "The name of the field that is missing a required value."))
  (:documentation "An error that signals a required value is missing."))


(defmethod print-object ((condition required-value-error) stream)
  (format stream "A value for field ~A is required but none was provided." (missing-field-name-of condition)))


(defclass nested-element ()
  ((element-type :type (or field symbol)
                 :initarg :element-type
                 :initform (error "A nested field requires an element-type to deserialize members to.")
                 :reader element-type-of
                 :documentation "The field that respresents the elements of the list.")))


(define-final-class list-field (field nested-element)
  ()
  (:documentation "A field that contains a list of values satsified by another field."))


(define-final-class nested-field (field nested-element)
  ()
  (:documentation "A field that represents a complex object located at this slot."))


(defmethod sanity-clause.protocol:deserialize ((field nested-field) value)
  (if (eq value :missing)
      value
      (sanity-clause.protocol:load (element-type-of field) value)))


(defmethod sanity-clause.protocol:deserialize ((field list-field) value)
  (etypecase value
    (missing
     value)

    (trivial-types:proper-list
     (with-slots (element-type) field
       (mapcar (lambda (item) (sanity-clause.protocol:load element-type item)) value)))))


(define-final-class map-field (field)
  ((key-field :initarg :key-field
              :accessor key-field-of
              :initform :string)
   (value-field :initarg :value-field
                :accessor value-field-of
                :initform :string))
  (:documentation "A field that maps values of one kind to values to another kind, like strings to strings, or numbers to objects.
examples::

  (make-field :map :key-field :string :value-field :integer)
  (deserialize * '((\"potato\" . 4) (\"chimp\" . 11)))
"))



(defmethod sanity-clause.protocol:resolve ((field map-field) data &optional parents)
  (let (accumulator)
    (with-slots (key-field value-field) field
      (sanity-clause.util:do-key-values (k v) data
        (push (cons (sanity-clause.protocol:resolve key-field k (list* key-field parents))
                    (sanity-clause.protocol:resolve value-field v (list* value-field parents)))
              accumulator)))
    accumulator))


(define-final-class one-field-of-field (field)
  ((field-choices :type list
                  :initarg :field-choices
                  :accessor field-choices-of
                  :initform (error ":field-choices is required in one-field-of-field.")
                  :documentation "Fields that this field could decode to."))
  (:documentation "A field type that allows any of the fields specified."))


(defmethod initialize-instance :after ((field one-field-of-field) &key)

  (labels ((ensure-subfield (field-args-or-field)
             (etypecase field-args-or-field
               ((or symbol cons)
                (apply #'make-field (make-args field-args-or-field)))

               (field
                ;; The sub-field needs the same data-key as the parent to get the data
                ;; might as well set attribute, too
                (with-slots (data-key attribute) field-args-or-field
                  (setf data-key (data-key-of field)
                        attribute (attribute-of field)))
                field-args-or-field)))

           (make-args (subfield-args)
             (destructuring-bind (type . args) (ensure-list subfield-args)
               ;; insert overridden args before the given ones so those values get
               ;; used instead of any later ones (as a result of how plists work).
               (list* type
                      :data-key (data-key-of field)
                      :attribute (attribute-of field)
                      (sanity-clause.validator:hydrate-validators args)))))

    (setf (field-choices-of field) (mapcar #'ensure-subfield (field-choices-of field))))

  field)


(defmethod sanity-clause.protocol:resolve ((field one-field-of-field) data &optional parents)
  (loop for (try-field . rest) on (field-choices-of field)
        ;; if the cdr of the field options is nil, we're out of other options
        for last-p = (not rest)

        do (handler-case (return (sanity-clause.protocol:resolve try-field data (list* field parents)))
             (t (e)
               (when last-p
                 (error 'conversion-error
                        :from-error e
                        :field field
                        :parents (reverse (list* field parents))))))))


(define-final-class one-schema-of-field (field)
  ((schema-choices :type list
                   :initarg :schema-choices
                   :accessor schema-choices-of
                   :documentation "Fields that this field could decode to."
                   :initform (error ":schema-choices is required in one-schema-of-field.")))
  (:documentation "A field type that allows any of the schemas specified."))


(defmethod initialize-instance :after ((field one-schema-of-field) &key)
  (flet ((ensure-class (symbol-or-class)
           (if (c2mop:classp symbol-or-class)
               symbol-or-class
               (find-class symbol-or-class))))

    (setf (schema-choices-of field) (mapcar #'ensure-class (schema-choices-of field))))
  field)


(defmethod sanity-clause.protocol:resolve ((field one-schema-of-field) data &optional parents)
  (loop for (try-schema . rest) on (schema-choices-of field)
        ;; if the cdr of the field options is nil, we're out of other options
        for last-p = (not rest)

        do (handler-case (return (sanity-clause.protocol:load try-schema data))
             (t (e) (when last-p (error 'conversion-error
                                        :from-error e
                                        :field field
                                        :parents (reverse (list* field parents))))))))
