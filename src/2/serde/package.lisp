(defpackage sanity-clause-2/serde
  (:use :cl)
  (:export
   #:serializable-metaclass
   #:serialize
   #:serialize-value
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

    value)

  (:method (serde (field sanity-clause-2::null-field) value)
    (if (alexandria:emptyp value)
        nil
        (error "~A is not an empty value" value)))

  (:method (serde (field sanity-clause-2::or-field) value)
    (loop :for field :in (sanity-clause-2::alternatives field)
          ;; Return the first that doesn't error
          :do (handler-case (return-from deserialize-value (deserialize-value serde field value))
                (error () nil))))

  (:method (serde (field sanity-clause-2::uri-field) (value string))
    (quri:uri value))

  (:method (serde (field sanity-clause-2::timestamp-field) (value string))
    (local-time:parse-timestring value))

  (:method (serde (field sanity-clause-2::hash-table-field) (value list))
    ;; Build with the 'equal test since deserialized keys will be strings and the like
    (loop :with table := (make-hash-table :test 'equal)
          :for (key . value) :in value
          :do (setf (gethash (deserialize-value serde (sanity-clause-2::key-field field) key) table)
                    (deserialize-value serde (sanity-clause-2::value-field field) value))
          :finally (return table))))


(defgeneric serialize-value (serde field value)
  (:documentation "Method responsible for taking different types of slot values and changing them to a value appropriate for the target format.")

  (:method (serde field value)
    (declare (ignore serde field))

    value)

  (:method (serde (field sanity-clause-2::uri-field) (value quri.uri:uri))
    (quri:render-uri value nil))

  (:method (serde (field sanity-clause-2::timestamp-field) (value local-time:timestamp))
    ;; Timestamps are written out in UTC unless the field's :serde options ask for a
    ;; timezone, either generically or under this serde type's key.
    (local-time:format-timestring
     nil value
     :format local-time:+iso-8601-format+
     :timezone (or (sanity-clause-2::get-serde-option
                    field (alexandria:make-keyword (class-name (class-of serde))) :timezone)
                   local-time:+utc-zone+)))

  (:method (serde (field sanity-clause-2::hash-table-field) (value hash-table))
    (loop :for key :being :the :hash-keys :of value
          :collect (cons (serialize-value serde (sanity-clause-2::key-field field) key)
                         (serialize-value serde (sanity-clause-2::value-field field) (gethash key value))))))


(defgeneric serialize (serde instance)
  (:documentation "Each serde type should be able to define this method and possibly call it repeatedly on nested objects."))


(defgeneric deserialize (serde class)
  (:documentation "Each serde instance should be able to define this method."))


(defgeneric slot-key (serde slot)
  (:method ((serde t) (slot sanity-clause-2::validated-slot-mixin))
    (sanity-clause-2::get-serde-option (sanity-clause-2::field slot)
                                       (alexandria:make-keyword (class-name (class-of serde)))
                                       :key)))
