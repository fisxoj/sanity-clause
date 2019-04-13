(defpackage :sanity-clause.metaclass.class
  (:use :cl :alexandria :cl-arrows)
  (:export #:validated-metaclass))

(in-package :sanity-clause.metaclass.class)

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


(defun initargs-for-slot (initargs)
  "Collects arguments for and initializes a field and then returns it along with the cleaned up :param:`initargs`.  This list can then be passed as the initargs to :class:`validated-slot-definition`."

  (multiple-value-bind (field-class field-initargs-from-type)
      ;; Use the value of :field-type preferentially to whatever we
      ;; might or might not have derived from :type
      (if-let ((field-type (getf initargs :field-type)))
        (sanity-clause.field:find-field field-type)
        (sanity-clause.metaclass.types:slot-type-to-field-initargs (getf initargs :type)))

    (multiple-value-bind (field-initargs others)
        (take-properties (class-initargs field-class) initargs)

      ;; Try to set data-key based on an initarg
      ;; FIXME: probably better to be smart about which initarg we use
      ;; or possibly refactor to allow many possible data sources.
      (let ((data-key (car (getf initargs :initargs))))

        (let ((field (apply #'sanity-clause.field:make-field
                            field-class
                            (list* :data-key data-key
                                   (merge-plist '(:validator) field-initargs-from-type field-initargs)))))

          ;; Remove this one special property, too, which isn't an initarg for either the
          ;; field or the class.
          (remove-from-plist others :field-type)

          (list* :field-instance field
                 others))))))


(defclass validated-metaclass (standard-class)
  ())


(defmethod make-instance :around ((class validated-metaclass) &rest initargs)

  (c2mop:ensure-finalized class)

  (let ((validated-initargs nil)
        (data-source (if (and (= 2 (length initargs))
                              (eq (first initargs) :source)
                              (eq (second initargs) :env))
                         :env
                         initargs)))

    (dolist (slot (c2mop:class-slots class))
      (let ((field (field-of slot)))

        (when (and (sanity-clause.field:load-field-p field)
                   ;; Don't bother trying to load something we don't have a data-key for
                   (sanity-clause.field:data-key-of field))
          (let ((value (->>
                        (sanity-clause.field:get-value field data-source)
                        (sanity-clause.field:deserialize field))))

            (sanity-clause.field:validate field value)

            (appendf validated-initargs
                     (list (sanity-clause.field:data-key-of field) value))))))

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
