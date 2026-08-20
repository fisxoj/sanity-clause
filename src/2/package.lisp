(defpackage sanity-clause-2
  (:use :cl)
  (:export
   #:metaclass
   #:define-validated-class
   #:deserialize-p
   #:serialize-p))

(in-package :sanity-clause-2)


(defparameter +fields+ (make-hash-table :test 'eq)
  "Namespace for fields")


(defclass metaclass (standard-class)
  ()
  (:documentation "Metaclass that provides the value checking."))


(defmacro define-validated-class (name direct-superclasses direct-slots &rest options)
  `(cl:defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options
     (:metaclass metaclass)))


(defmethod closer-mop:validate-superclass ((class metaclass) (superclass standard-class))
  (declare (ignore class superclass))
  t)


(defmethod closer-mop:direct-slot-definition-class ((class metaclass) &rest initargs)
  (declare (ignore class initargs))
  'direct-validated-slot)


(defmethod closer-mop:effective-slot-definition-class ((class metaclass) &rest initargs)
  (declare (ignore class initargs))
  'effective-validated-slot)


(defvar *path* nil
  "Keeps track of the current path in the case of nested classes.")



(defmacro with-path ((new-path-segment) &body body)
  (alexandria:once-only (new-path-segment)
    `(let ((*path* (append *path* (list ,new-path-segment))))
       ,@body)))


(defun current-path ()
  *path*)


(defgeneric validate (field value))


(defclass field ()
  ((required :type          boolean
             :initarg       :required
             :initform      t
             :reader        required
             :documentation "If true and no value is found for the field, it is considered a validation error.")
   (serialize :type          boolean
              :initarg       :serialize
              :initform      t
              :reader        serialize-p
              :documentation "Indicates the value should be written out by serializers.")
   (deserialize :type          boolean
                :initarg       :deserialize
                :initform      t
                :reader        deserialize-p
                :documentation "Indicates the value should be read into the object when deserializing.")
   (serde-options :type          list
                  :initarg       :serde
                  :initform      nil
                  :reader        serde-options
                  :documentation "Configuration options for serde operations.  A free form plist-field."))
  (:documentation "Base field type."))


;; TODO: Move serde options into the serde package and maybe have a
;; slightly different slot mixin defined there?
(defun get-serde-option (field serde-key option)
  (or (getf (getf (serde-options field) serde-key) option)
      (getf (serde-options field) option)))


(defclass validated-slot-mixin ()
  ((field :type          field
          :accessor      field
          :initarg       :field
          :documentation "The class that provides validation for a given slot in the class.")))


(defclass direct-validated-slot (closer-mop:standard-direct-slot-definition validated-slot-mixin)
  ())


(defclass effective-validated-slot (closer-mop:standard-effective-slot-definition validated-slot-mixin)
  ())


(defmacro define-field (name direct-superclasses direct-slots &rest options)
  (let ((field-name (intern (format nil "~a-FIELD" name))))
    `(progn
       (defclass ,field-name ,direct-superclasses ,direct-slots ,@options)
       (setf (gethash ',name +fields+) (find-class ',field-name)))))


(define-field integer (field)
  ((maximum :type (or null integer)
            :initarg :maximum
            :reader maximum
            :initform nil)
   (minimum :type (or null integer)
            :initarg :minimum
            :reader minimum
            :initform nil)))


(define-condition validation-error ()
  ((value :initarg :value)
   (condition :initarg :condition))
  (:report (lambda (c s)
             (with-slots (value condition) c
               (format s "value ~a must ~a" value condition)))))


(defmethod validate ((field integer-field) value)
  (unless (typep value 'integer)
    (error 'validation-error :value value
                             :condition "be of type integer"))

  (when (maximum field)
    (unless (<= value (maximum field))
      (error 'validation-error :value value
                               :condition (format nil "be less than or equal to ~d" (maximum field)))))

  (when (minimum field)
    (unless (>= value (minimum field))
      (error 'validation-error :value value
                               :condition (format nil "be greater than or equal to ~d" (minimum field))))))




(defmethod print-object ((field integer-field) stream)
  (print-unreadable-object (field stream :type t :identity nil)
    (with-slots (minimum maximum required) field
      (format stream "minimum: ~a maximum: ~a required: ~a"
              minimum
              maximum
              required))))


(define-field string (field)
  ((length :type (or null (integer 0))
           :initarg :length
           :reader string-length
           :documentation "The length of the string.")))


(defmethod validate ((field string-field) value)
  (unless (typep value 'string)
    (error 'validation-error :value value
                             :condition "be a string value.")))


(define-field boolean (field)
  ())


(defmethod validate ((field boolean-field) value)
  (unless (typep value 'boolean)
    (error 'validation-error :value value
                             :condition "be a boolean value.")))


(define-field forward-reference (field)
  ((referenced-class :type (or symbol class)
                     :initarg :class
                     :reader forward-referenced-class
                     :documentation "The class to be loaded with data for this field.")))


(defmethod validate ((field forward-reference-field) value)
  (unless (typep value (forward-referenced-class field))
    (error 'validation-error :value value
                             :condition (format nil "be of type ~A" (forward-referenced-class field)))))


(define-field float (field)
  ((maximum :type (or null float)
            :initarg :maximum
            :reader maximum)
   (minimum :type (or null float)
            :initarg :minimum
            :reader minimum)))


(defmethod validate ((field float-field) value)
  (unless (typep value 'float)
    (error 'validation-error :value value
                             :condition "be of type float"))

  (when (maximum field)
    (unless (<= value (maximum field))
      (error 'validation-error :value value
                               :condition (format nil "be less than or equal to ~d" (maximum field)))))

  (when (minimum field)
    (unless (>= value (minimum field))
      (error 'validation-error :value value
                               :condition (format nil "be greater than or equal to ~d" (minimum field))))))


(define-field or (field)
  ((alternatives :type list
                 :initarg :alternatives
                 :reader alternatives)))


(define-condition or-validation-error (validation-error)
  ((failed-validations :initarg :failed-validations))
  (:report (lambda (c s)
             (with-slots (failed-validations value) c
               (format s "value ~a must satisfy any of the following conditions, but did not:~%~{* ~a~%~}" value failed-validations)))))


(defmethod shared-initialize ((field or-field) slot-names &rest initargs &key alternatives)
  (apply #'call-next-method field slot-names :alternatives (mapcar 'make-field alternatives) initargs))


(defmethod validate ((field or-field) value)
  (loop :with failed-validations := nil
        :for sub-field :in (alternatives field)

        ;; If ANY validation succeeds, return from the method,
        ;; otherwise collect the errors and return them.
        :do (handler-case (progn (validate sub-field value)
                                 (return-from validate))
              (validation-error (e) (push e failed-validations)))
        :finally (error 'or-validation-error :value value
                                             :failed-validations failed-validations)))


(define-field list (field)
  ((element-type :initarg :element-type
                 :reader element-type)))


(defmethod shared-initialize ((field list-field) slot-names &rest initargs &key element-type)
  (apply #'call-next-method field slot-names :element-type (apply #'make-field element-type)
         initargs))


(define-condition list-validation-error (validation-error)
  ((failed-validation :initarg :failed-validation)
   (index :initarg :index))
  (:report (lambda (c s)
             (with-slots (failed-validation value index) c
               (format s "value ~a at index ~d must satisfy the following condition, but did not: ~a" value index failed-validation)))))


(defmethod validate ((field list-field) values)
  (unless (typep values 'list)
    (error 'validation-error :condition "be a sequence" :value values))

  (loop :for value :in values
        :for index :from 0 :by 1

        :do (handler-case (validate (slot-value field 'element-type) value)
              (validation-error (e)
                (error 'list-validation-error :failed-validation e
                                              :condition "be valid for the element-type"
                                              :index index) ))))


(define-field null (field)
  ())


(defmethod validate ((field null-field) value)
  (unless (null value)
    (error 'validation-error :condition "be null" :value value)))


(defun infer-field-type (type)
  (trivia:match type
    ((list* head rest)
     (apply #'field-from-typespec head rest))
    (_ (field-from-typespec type))))


(defun make-field (field &rest initargs)
  (let ((field-class (gethash field +fields+)))
    (if field-class
        (apply #'make-instance field-class initargs)
        (error "No field defined with name ~S" field))))


;; TODO: Split these out next to the field definitions
(defgeneric field-from-typespec (head &rest rest)
  (:method ((head (eql 'integer)) &rest rest)
    (destructuring-bind (&optional (minimum '*) (maximum '*)) rest
        (make-field 'integer :minimum (unless (eq minimum '*) minimum)
                             :maximum (unless (eq maximum '*) maximum))))

  (:method ((head (eql 'float)) &rest rest)
    (destructuring-bind (&optional (minimum '*) (maximum '*)) rest
      (make-field 'float :minimum (unless (eq minimum '*) minimum)
                         :maximum (unless (eq maximum '*) maximum))))

  (:method ((head (eql 'string)) &rest rest)
    (destructuring-bind (&optional (length '*)) rest
      (make-field 'string :length (unless (eq length '*) length))))

  (:method ((head (eql 'boolean)) &rest rest)
    (declare (ignore rest))

    (make-field 'boolean))

  (:method ((head symbol) &rest rest)
    (declare (ignore rest))

    (make-instance 'forward-reference-field :class head))

  (:method ((head (eql 'or)) &rest alternatives)
    (make-field 'or :alternatives alternatives))

  (:method ((head (eql 'null)) &rest rest)
    (declare (ignore rest))

    (make-field 'null))

  (:method ((head (eql 'list)) &rest rest)

    (make-field 'list :element-type rest))

  (:documentation "Attempts to create a field from a valid lisp typespec.  Any field that can't be represented this way should be constructed using the :field initarg instead of the :type initarg to a slot."))


(defmethod shared-initialize ((slot validated-slot-mixin)
                              slot-names
                              &rest
                                initargs
                              &key
                                field
                                type
                                (serialize t serialize-present)
                                (deserialize t deserialize-present)
                                (serde nil serde-present)
                                (required t required-present)
                              &allow-other-keys)

  (assert (not (and type field)) (type field) "Don't specify values for both :type and :field on a validated slot.  Received ~a for :type and ~a for :field" type field)

  ;; Set initargs and other things but without our initargs for fields
  (apply #'call-next-method slot slot-names :type type initargs)

  (let ((field (if field
                   (if (consp field)
                       (apply #'make-field
                              (car field)
                              :serialize serialize
                              :deserialize deserialize
                              :serde serde
                              (cdr field))
                       (make-field field))
                   (infer-field-type type))))

    (setf (slot-value slot 'field) field)

    (when serde-present
      (setf (slot-value field 'serde-options) serde))

    ;; FIXME: cont'd - these values are boolean and should default to
    ;; true, so we need to check that they were specified or not to do
    ;; the right thing.
    (when serialize-present
      (setf (slot-value field 'serialize) serialize))

    (when required-present
      (setf (slot-value field 'required) required))

    (when deserialize-present
      (slot-value field 'deserialize) deserialize)))


(defmethod c2mop:compute-effective-slot-definition :around ((class metaclass) name direct-slot-definitions)

  ;; Make sure we set the field to the one defined on the most-specific (left-most)
  ;; direct slot definition inherited by this class.
  (let ((effective-slot (call-next-method)))

    (setf (field effective-slot) (some #'field direct-slot-definitions))

    effective-slot))


(defmethod (setf c2mop:slot-value-using-class) (value (class metaclass) object slot-definition)
  (validate (field slot-definition) value)
  (call-next-method))
