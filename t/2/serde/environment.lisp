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
