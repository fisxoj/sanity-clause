(defpackage :sanity-clause-2/serde/test
  (:local-nicknames (:put :sanity-clause-2/serde))
  (:use #:cl
        #:parachute))

(in-package #:sanity-clause-2/serde/test)


(defclass job-configuration ()
  ((queue-name :type string) ;; JOBS___QUEUE_NAME
   (workers :type (integer 0)))
  (:metaclass sanity-clause-2/serde:serializable-metaclass)
  (:serde-options :environment (:separator "___")))


(defclass configuration ()
  ((debug :type boolean)
   (job-configuration :type  job-configuration
                      :serde (:environment (:key "JOBS"))))
  (:metaclass sanity-clause-2/serde:serializable-metaclass))


(define-test environment
  (setf (uiop:getenv "JOBS___QUEUE_NAME") "billy"
        (uiop:getenv "JOBS___WORKERS") "5"
        (uiop:getenv "DEBUG") "true")

  (let ((configuration (put:deserialize (make-instance 'sanity-clause-2/serde::environment) 'configuration)))

    (of-type configuration configuration)
    (is eq (slot-value configuration 'debug) t)
    (of-type job-configuration (slot-value configuration 'job-configuration))
    (is string= "billy" (slot-value (slot-value configuration 'job-configuration) 'queue-name))
    (is = 5 (slot-value (slot-value configuration 'job-configuration) 'workers))))


;; TODO:
;; - required values
;; - default values
;; - initargs
;; - initforms
