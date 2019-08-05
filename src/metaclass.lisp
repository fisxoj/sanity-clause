(defpackage :sanity-clause.metaclass
  (:use :cl :alexandria :cl-arrows)
  (:export #:validated-metaclass)
  (:documentation "The :class:`validated-metaclass` is a way of defining classes that have contracts enforced for them.

::

   (defclass person ()
        ((favorite-dog :type symbol
                       :field-type :member
                       :members (:wedge :walter)
                       :initarg :favorite-dog
                       :required t)
         (age :type (integer 0)
              :initarg :age
              :required t)
         (potato :type string
                 :initarg :potato
                 :required t))
        (:metaclass sanity-clause.metaclass:validated-metaclass))

The above defines a class that can be instantiated with :function:`make-instance`, but will error if the initargs don't satisfy the contract required by the field.

Some special types can be specifed with lisp type specs, like ``age`` above, which will generate an :class:`sanity-clause.field:integer-field`, with validations requiring the value be at least 0.

**Nota Bene:** At the moment, there is no validation done on updating slots after instances are created."))

(in-package :sanity-clause.metaclass)

(defun class-initargs (class)
  "Collect the initargs for :param:`class`, which is either a class or a symbol naming a class."

  (unless (c2mop:classp class)
    (setf class (find-class class)))

  (->
   (loop
     for slot in (c2mop:class-slots class)
     collecting (c2mop:slot-definition-initargs slot))
   alexandria:flatten))


(defun take-properties (take-list from)
  "Takes keys from :param:`take-list` from the provided plist :param:`from`."

  (let (found others)
    (doplist (k v from)
             (if (member k take-list)
                 (appendf found (list k v))
                 (appendf others (list k v))))

    (values found others)))


(defun merge-plist (keys list1 list2)
  "Merge the keys :param:`keys` on :param:`list1` and :param:`list2`."

  (multiple-value-bind (take1 others1) (take-properties keys list1)
    (multiple-value-bind (take2 others2) (take-properties keys list2)
      (append (loop for key in keys
                    appending (list key (append (getf take1 key) (getf take2 key))))
              others1
              others2))))


(defun slot-type-to-field-initargs (typespec)
  "Sometimes, it's useful to try to infer the field type from a lisp type-spec.  For example::

    (integer 0 10)

  should produce a field that expects integers ``(<= 0 n 10)``.

In the event the type isn't a simple type, assume it's a class with metaclass :class:`validated-metaclass` and try to use that instead."

  (let ((typespec (ensure-list typespec)))
    (case (car typespec)
      (integer
       (values (find-class 'sanity-clause.field:integer-field)
               (list :validator (list (lambda (v) (sanity-clause.validator:int v :min (second typespec) :max (third typespec)))))))

      (string
       (values (find-class 'sanity-clause.field:string-field)
               (when-let ((length (second typespec)))
                 (list :validator (list (lambda (v) (sanity-clause.validator:str v :max-length length :min-length length)))))))

      (real
       (values (find-class 'sanity-clause.field:real-field) nil))

      ;; Try to identify classes that are validated-metaclass classes, which can be used as fields
      ;; when composing classes with others.
      (otherwise
       (if-let ((field-class (find-class (car typespec) nil)))
         (when (eq (class-of field-class) (find-class 'sanity-clause.metaclass:validated-metaclass))
           field-class))))))


(defun initargs-for-slot (initargs)
  "Collects arguments for and initializes a field and then returns it along with the cleaned up :param:`initargs`.  This list can then be passed as the initargs to :class:`validated-field-slot-definition`."

  (multiple-value-bind (field-class field-initargs-from-type)
      ;; Use the value of :field-type preferentially to whatever we
      ;; might or might not have derived from :type
      (if-let ((field-type (getf initargs :field-type)))
        (sanity-clause.field:find-field field-type)
        (slot-type-to-field-initargs (getf initargs :type)))

    (multiple-value-bind (field-initargs others)
        (take-properties (class-initargs field-class) initargs)

      ;; Try to set data-key based on an initarg
      ;; FIXME: probably better to be smart about which initarg we use
      ;; or possibly refactor to allow many possible data sources.
      (let* ((data-key (first (getf initargs :initargs)))
             (field (apply #'sanity-clause.field:make-field
                            field-class
                            (list* :data-key (getf initargs :data-key data-key)
                                   (merge-plist '(:validator) field-initargs-from-type field-initargs)))))

        ;; Remove this one special property, too, which isn't an initarg for either the
        ;; field or the class.
        (remove-from-plist others :field-type)

        (list* :field-instance field
               others)))))


(defclass validated-metaclass (standard-class)
  ())


(defmethod make-instance :around ((class validated-metaclass) &rest initargs)

  (c2mop:ensure-finalized class)

  (let ((validated-initargs nil)
        ;; There's one special case here to read values from environment variables if the &rest args
        ;; passed here are '(:source :env)
        (data-source (if (and (= 2 (length initargs))
                              (eq (first initargs) :source)
                              (eq (second initargs) :env))
                         :env
                         initargs)))

    (dolist (slot (c2mop:class-slots class))

      (let* ((field (field-of slot))
             (initarg (first (c2mop:slot-definition-initargs slot))))

        (when (and (sanity-clause.field:load-field-p field)
                   ;; Don't bother trying to load something we don't have a data-key for
                   (sanity-clause.field:data-key-of field))

          (let ((value (->>
                        (sanity-clause.util:get-value data-source initarg (sanity-clause.field::default-of (field-of slot)))
                        (sanity-clause.field:deserialize field))))

            (sanity-clause.field:validate field value)

            (appendf validated-initargs
                     (list initarg value))))))

    (apply #'call-next-method class validated-initargs)))


(defmethod c2mop:validate-superclass ((mc validated-metaclass) (c standard-object))
  (declare (ignore c mc))

  t)


(defclass validated-slot-definition ()
  ((field :accessor field-of
          :type sanity-clause.field:field
          :initarg :field-instance)))


(defclass validated-direct-slot-definition (validated-slot-definition
                                            c2mop:standard-direct-slot-definition)
  ())


(defclass validated-effective-slot-definition (validated-slot-definition
                                               c2mop:standard-effective-slot-definition)
  ())


(defmethod c2mop:direct-slot-definition-class ((class validated-metaclass) &key)

  'validated-direct-slot-definition)


(defmethod c2mop:effective-slot-definition-class ((class validated-metaclass) &key)

  'validated-effective-slot-definition)


(defmethod shared-initialize :around ((slot validated-direct-slot-definition) slot-names &rest initargs &key &allow-other-keys)

  ;; This cleverly extracts the initargs for the field that will be stored on the slot
  ;; and returns the other arguments that are the usual initargs for a standard-direct-slot
  (apply #'call-next-method slot slot-names (initargs-for-slot initargs)))


(defmethod c2mop:compute-effective-slot-definition :around ((class validated-metaclass) name direct-slot-definitions)

  ;; Make sure we set the field to the one defined on the most-specific (left-most)
  ;; direct slot definiton inherited by this class.
  (let ((effective-slot (call-next-method)))

    (setf (field-of effective-slot) (some #'field-of direct-slot-definitions))

    effective-slot))


;;; Code to add classes to the protocol for list-based schemas sanity-clause.schema:load
;;; This is useful for making classes from alists (ie. not using the ``make-instance`` interface.


(defmethod sanity-clause.field:get-value ((slot validated-slot-definition) object &optional field-name)
  (declare (ignore field-name))

  (sanity-clause.field:get-value (field-of slot) object))


(defun collect-initargs-from-list (class data)
  "Converts an input list from form ``([data-key] [value] ...)`` or ``(([data-key] . [value]) ...)`` to ``([initarg] [value] ...)`` so it is suitable for passing to :function:`make-instance`."

  (declare (type validated-metaclass class)
           (type (or trivial-types:property-list trivial-types:association-list) data))

  (c2mop:ensure-finalized class)

  (loop for slot in (c2mop:class-slots class)
        when (sanity-clause.field:load-field-p (field-of slot))
          collecting (first (c2mop:slot-definition-initargs slot))
        collecting (sanity-clause.field:get-value slot data)))


(defmethod sanity-clause.schema:load ((symbol symbol) data &optional format)
  (declare (ignore format))

  (sanity-clause.schema:load (find-class symbol) data))


(defmethod sanity-clause.schema:load ((class validated-metaclass) data &optional format)
  (declare (ignore format))

  (apply #'make-instance class (collect-initargs-from-list class data)))


;; For fields that inherit from nested-element, this behavior is a
;; bit circular.  The value should be the initargs for an inner
;; schema.


(defclass nested-element ()
  ((element-type :type (or field symbol)
                 :initarg :element-type
                 :initform (error "A nested field requires an element-type to deserialize members to.")
                 :reader element-type-of
                 :documentation "The field that respresents the elements of the list.")))


(sanity-clause.field::define-final-class list-field (sanity-clause.field::field nested-element)
  ()
  (:documentation "A field that contains a list of values satsified by another field."))


(sanity-clause.field::define-final-class nested-field (sanity-clause.field::field nested-element)
  ()
  (:documentation "A field that represents a complex object located at this slot."))


(import '(list-field nested-field) (find-package 'sanity-clause.field))


(export '(list-field nested-field) (find-package 'sanity-clause.field))


(defmethod sanity-clause.field:deserialize ((field nested-field) value)
  (sanity-clause.schema:load (element-type-of field) value))


(defmethod sanity-clause.field:deserialize ((field list-field) value)
  (etypecase value
    (trivial-types:proper-list
     (with-slots (element-type) field
       (mapcar (lambda (item) (sanity-clause.schema:load element-type item)) value)))))
