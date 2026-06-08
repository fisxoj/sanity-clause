(defpackage sanity-clause-2/serde
  (:use :cl)
  (:export
   #:serializable-metaclass
   #:serialize
   #:deserialize
   #:deserialize-value))

(in-package :sanity-clause-2/serde)


(defclass serializable-metaclass (sanity-clause-2:metaclass)
  ((options :initarg  :serde-options
            :reader   options
            :initform '(:separator "__")
            :documentation "Expects a plist of serde types and their options.  Some options can be defined generically and the serde specific option will be preferred.

::
  (defclass potato ()
    ((type :type (member :yam :idaho)))
    (:metaclass serializable-class)
    (:serde-options :environment (:separator \"__\")
                    :json (:key-mangler camel-case)))")))


(defun get-serde-option (class serde-key option)
  (or (getf (getf (options class) serde-key) option)
      (getf (options class) option)))


(defgeneric deserialize-value (serde field value)
  (:documentation "Method responsible for taking different types of values and optionally converting them before treating them as the value given to a slot.  Useful for deserializing especially.")

  (:method (serde field value)
    (declare (ignore serde field))

    value))


(defgeneric serialize-value (serde field value)
  (:documentation "Method responsible for taking different types of slot values and changing them to a value appropriate for the target format.")

  (:method (serde field value)
    (declare (ignore serde field))

    value))


(defgeneric serialize (serde instance)
  (:documentation "Each serde type should be able to define this method and possibly call it repeatedly on nested objects."))


(defgeneric deserialize (serde class)
  (:documentation "Each serde instance should be able to define this method."))
