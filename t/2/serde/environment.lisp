(defpackage :sanity-clause-2/serde/test
  (:local-nicknames (:put :sanity-clause-2/serde))
  (:use #:cl
        #:parachute))

(in-package #:sanity-clause-2/serde/test)


(defclass job-configuration ()
  ((queue-name :type string) ;; JOBS___QUEUE_NAME
   (workers :type (integer 0)
            ;; different name than slot name
            :serde (:environment (:key "WORKER_COUNT"))))
  (:metaclass sanity-clause-2/serde:serializable-metaclass)
  (:serde-options :environment (:separator "___")))


(defclass configuration ()
  ((debug :type boolean)
   (job-configuration :type  job-configuration
                      :serde (:environment (:key "JOBS"))))
  (:metaclass sanity-clause-2/serde:serializable-metaclass))


(defclass configuration2 ()
  ((debug :type boolean)
   (job-configuration :type job-configuration))
  (:metaclass sanity-clause-2/serde:serializable-metaclass))


(define-test environment
  (setf (uiop:getenv "JOBS___QUEUE_NAME") "billy"
        (uiop:getenv "JOBS___WORKER_COUNT") "5"
        (uiop:getenv "DEBUG") "true")

  (let ((configuration (put:deserialize (make-instance 'sanity-clause-2/serde::environment) 'configuration)))

    (of-type configuration configuration)
    (is eq (slot-value configuration 'debug) t)
    (of-type job-configuration (slot-value configuration 'job-configuration))
    (is string= "billy" (slot-value (slot-value configuration 'job-configuration) 'queue-name))
    (is = 5 (slot-value (slot-value configuration 'job-configuration) 'workers)))


  ;; Checks default slot keys name getting correctly incorporated into longer env var names
  (setf (uiop:getenv "JOB_CONFIGURATION___QUEUE_NAME") "gwen"
        (uiop:getenv "JOB_CONFIGURATION___WORKER_COUNT") "3")
  (let ((configuration (put:deserialize (make-instance 'sanity-clause-2/serde::environment) 'configuration2)))
    (of-type configuration2 configuration)
    (is eq (slot-value configuration 'debug) t)
    (of-type job-configuration (slot-value configuration 'job-configuration))
    (is string= "gwen" (slot-value (slot-value configuration 'job-configuration) 'queue-name))
    (is = 3 (slot-value (slot-value configuration 'job-configuration) 'workers))))


;; TODO:
;; - required values
;; - default values
;; - initargs
;; - initforms
(define-test field-conversions
  (let ((serde (make-instance 'sanity-clause-2/serde::environment))
        (timestamp-field (sanity-clause-2::make-field 'timestamp))
        (uri-field (sanity-clause-2::make-field 'uri))
        (hash-table-field (sanity-clause-2::make-field 'hash-table :key-field 'string :value-field 'integer)))

    ;; Timestamp strings become timestamps and survive a round trip back to a string
    (let ((value (put:deserialize-value serde timestamp-field "2026-04-25T10:00:00Z")))
      (of-type local-time:timestamp value)
      (is local-time:timestamp= value
          (put:deserialize-value serde timestamp-field
                                 (put:serialize-value serde timestamp-field value))))

    ;; URI strings become uris and serialize back to strings
    (let ((value (put:deserialize-value serde uri-field "https://example.com/potato")))
      (of-type quri.uri:uri value)
      (is string= "https://example.com/potato"
          (put:serialize-value serde uri-field value)))

    ;; Alists become equal-test hash tables and back to alists again
    (let ((value (put:deserialize-value serde hash-table-field '(("potato" . 4) ("chimp" . 11)))))
      (of-type hash-table value)
      (is equalp '(("chimp" . 11) ("potato" . 4))
          (sort (put:serialize-value serde hash-table-field value) #'string< :key #'car)))))


(defun timezone-or-load (name)
  "Find a named timezone, reading the system tz repository in if it hasn't been loaded yet."
  (or (ignore-errors (local-time:find-timezone-by-location-name name))
      (progn
        (local-time:reread-timezone-repository :timezone-repository "/usr/share/zoneinfo/")
        (local-time:find-timezone-by-location-name name))))


(define-test timestamp-timezone
  (let ((serde (make-instance 'sanity-clause-2/serde::environment))
        (timestamp (local-time:parse-timestring "2026-04-25T10:00:00Z"))
        (new-york (timezone-or-load "America/New_York"))
        (kolkata (timezone-or-load "Asia/Kolkata")))

    ;; Timestamps serialize in UTC unless the field asks for another timezone
    (is string= "2026-04-25T10:00:00.000000Z"
        (put:serialize-value serde (sanity-clause-2::make-field 'timestamp) timestamp))

    ;; A field's timezone applies across serdes
    (is string= "2026-04-25T06:00:00.000000-04:00"
        (put:serialize-value serde
                             (sanity-clause-2::make-field 'timestamp :serde (list :timezone new-york))
                             timestamp))

    ;; The serde-specific option wins over the generic one
    (is string= "2026-04-25T15:30:00.000000+05:30"
        (put:serialize-value serde
                             (sanity-clause-2::make-field 'timestamp
                              :serde (list :timezone local-time:+gmt-zone+
                                           :environment (list :timezone kolkata)))
                             timestamp))))
