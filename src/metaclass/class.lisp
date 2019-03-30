(defpackage :sanity-clause.metaclass.class
  (:use :cl :alexandria :cl-arrows)
  (:export #:validated-metaclass))

(in-package :sanity-clause.metaclass.class)

(defclass validated-slot-definition (c2mop:standard-direct-slot-definition)
  ((field :accessor field-of
          :type sanity-clause.field:field
          :initarg :field-instance)))


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

      (let ((field (apply #'sanity-clause:make-field
                          field-class
                          (merge-plist '(:validator) field-initargs-from-type field-initargs))))

        ;; Remove this one special property, too, which isn't an initarg for either the
        ;; field or the class.
        (remove-from-plist others :field-type)

        (list* :field-instance field
               others)))))


(defmethod shared-initialize :around ((slot validated-slot-definition) slot-names &rest initargs &key &allow-other-keys)

  (apply #'call-next-method slot slot-names (initargs-for-slot initargs)))


(defclass validated-metaclass (standard-class)
  ())


(defmethod c2mop:validate-superclass ((mc validated-metaclass) (c standard-object))
  (declare (ignore c mc))

  t)


(defmethod c2mop:direct-slot-definition-class ((class validated-metaclass) &key)

  'validated-slot-definition)
