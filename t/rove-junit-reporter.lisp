(defpackage :rove/reporter/junit
  (:nicknames :rove-junit)
  (:use #:cl
        #:rove/reporter
        #:rove/core/stats
        #:rove/core/result
        #:rove/misc/stream)
  (:export #:run
           #:*junit-output*))

(in-package :rove/reporter/junit)


(defvar +max-stack-trace-depth+ 15
  "Take at most this many stack frames for error output.")


(defvar *junit-output* nil
  "The output stream for junit xml.")


(defvar *test-name* nil
  "The name of the test being executed.")


(defvar *test-depth* 0
  "The level of test nesting.  It seems like there isn't another way to tell when we are inside a package, deftest, or testing form.  We assume 1 = package, 2 = deftest, 3+ = testing (0 is uninitialized).")


(defun assertion-duration-seconds (assertion)
  "Convert rove's ms durations (that might be nil) to seconds."

  (/ (or (assertion-duration assertion) 0) 1000))


(defclass junit-reporter (rove/reporter/spec:spec-reporter)
  ((junit-stream :accessor junit-stream-of
                 :initarg :junit-stream)
   (test-description :accessor test-description-of
                     :initform nil)))


(defun test-description (reporter assertion)
  (format nil "~{~a~^ ~}"
          (reverse (cons (assertion-description assertion) (test-description-of reporter)))))


;; (defclass junit-spec-reporter (junit-reporter rove/reporter/spec:spec-reporter)
;;   ())


;; (defclass junit-dot-reporter (junit-reporter rove/reporter/dot:dot-reporter)
;;   ())


(defun xml-escape-printer (stream string at colon)
  "\"  &quot;
'   &apos;
<   &lt;
>   &gt;
&   &amp;"

  (declare (ignore at colon))

  (loop for char across string
        do (cond
             ((char= char #\") (format stream "&quot;"))
             ((char= char #\') (format stream "&apos;"))
             ((char= char #\<) (format stream "&lt;"))
             ((char= char #\>) (format stream "&gt;"))
             ((char= char #\&) (format stream "&amp;"))
             (t (princ char stream)))))


(defmethod initialize-instance :after ((reporter junit-reporter) &rest initargs &key stream junit-stream &allow-other-keys)
  (declare (ignore initargs))

  (when stream
    (setf (junit-stream-of reporter)
          (make-indent-stream stream)))

  (unless junit-stream
    (setf (junit-stream-of reporter)
          (make-indent-stream *junit-output*))))


(defmethod record ((reporter junit-reporter) (object failed-assertion))
  (call-next-method)

  (let ((stream (junit-stream-of reporter)))

    (with-indent (stream +2)
      (fresh-line stream)
      (format stream
              "<testcase name=\"~/rove-junit::xml-escape-printer/\" ~
                         classname=\"~/rove-junit::xml-escape-printer/\" ~
                         time=\"~f\">"
              (test-description reporter object)
              *test-name*
              (assertion-duration object))

      (with-indent (stream +2)
        (fresh-line stream)
        (if (null (assertion-stacks object))
            ;; a simple test failure
            (progn
              (format stream "<failure type=\"~/rove-junit::xml-escape-printer/\">~/rove-junit::xml-escape-printer/</failure>"
                      (string (type-of (assertion-reason object)))
                      (string (assertion-reason object))))
            ;; an error with a backtrace
            (progn
              (format stream "<error type=\"~/rove-junit::xml-escape-printer/\">~{~/rove-junit::xml-escape-printer/~^~%~}</error>"
                      (string (type-of (assertion-reason object)))
                      (mapcar (lambda (stack) (dissect:present-object stack nil))
                              (subseq (assertion-stacks object) 0 +max-stack-trace-depth+))))))
      (fresh-line stream)
      (format stream "</testcase>"))))


(defmethod record ((reporter junit-reporter) (object passed-assertion))
  (call-next-method)

  (let ((stream (junit-stream-of reporter)))
    (fresh-line stream)
    (format stream
            "<testcase name=\"~/rove-junit::xml-escape-printer/\" ~
                       classname=\"~/rove-junit::xml-escape-printer/\" ~
                       time=\"~d\"></testcase>"
            (test-description reporter object)
            *test-name*
            (assertion-duration object))))


(defmethod record ((reporter junit-reporter) (object pending-assertion))
  (call-next-method)

  (let ((stream (junit-stream-of reporter)))
    (fresh-line stream)
    (format stream
            "<testcase name=\"~/rove-junit::xml-escape-printer/\" ~
                       classname=\"~/rove-junit::xml-escape-printer/\" ~
                       time=\"0\">"
            (test-description reporter object)
            *test-name*)
    (with-indent (stream +2)
      (fresh-line stream)
      (format stream "<skipped />"))
    (fresh-line stream)
    (format stream "</testcase>")))


(defmethod test-begin ((reporter junit-reporter) test-name &optional count)
  (declare (ignore count))

  (unless (toplevel-stats-p reporter)
    (push test-name (test-description-of reporter)))

  (incf *test-depth*)

  (when (= 2 *test-depth*)
    (setf *test-name* test-name))

  (when (<= *test-depth* 2)
    (let ((stream (junit-stream-of reporter)))
      (fresh-line stream)
      (if (toplevel-stats-p reporter)
          (format stream "<testsuites>")
          (format stream
                  "<testsuite name=\"~/rove-junit::xml-escape-printer/\" ~
                            timestamp=\"~a\" ~
                            time=\"~d\" ~
                            hostname=\"~a\" ~
                            failures=\"~d\" ~
                            errors=\"~d\" ~
                            skipped=\"~d\" ~
                            tests=\"~d\">"
                  test-name
                  (local-time:format-timestring nil (local-time:now)
                                                :timezone local-time:+utc-zone+)
                  (+ (length (stats-failed reporter))
                      (length (stats-passed reporter))
                      (length (stats-pending reporter)))
                  (uiop:hostname)
                  (length (stats-failed reporter))
                  (length (remove-if #'null (stats-failed reporter)
                                     :key #'assertion-stacks))
                  (length (stats-pending reporter))
                  (context-test-count reporter)))

      (incf (stream-indent-level stream) 2)))

  (call-next-method))


(defmethod test-finish ((reporter junit-reporter) test-name)
  (multiple-value-bind (passedp context) (call-next-method)
    (declare (ignore context))

    (unless (toplevel-stats-p reporter)
      (pop (test-description-of reporter)))

    (when (<= *test-depth* 2)
      (let ((stream (junit-stream-of reporter)))

        (decf (stream-indent-level stream) 2)
        (fresh-line stream)

        (if (toplevel-stats-p reporter)
            (format stream "</testsuites>")
            (format stream "</testsuite>"))))

    (decf *test-depth*)

    passedp))


(defun run (component &key (env rove:*default-env*) (style rove/main:*default-reporter*))
  (labels ((junit-enabled-p ()
             (uiop:getenvp "JUNIT_ENABLED"))

           (run-tests ()
             (rove:run component
                       :style (if (junit-enabled-p)
                                  :junit
                                  style)
                       :env env))

           (maybe-with-junit-stream (thunk)
             (if (junit-enabled-p)
                 (with-open-file (stream (if (uiop:getenvp "JUNIT_REPORT_FILENAME")
                                             (uiop:getenv "JUNIT_REPORT_FILENAME")
                                             "report.xml")
                                         :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :supersede)
                   (let ((*junit-output* stream))
                     (funcall thunk)))
                 (funcall thunk)))

           (maybe-quit-if-fail (thunk)
             (if (uiop:getenvp "GITHUB_ACTIONS")
                 (or (funcall thunk) (uiop:quit -1))
                 (funcall thunk))))

    (maybe-quit-if-fail (lambda () (maybe-with-junit-stream #'run-tests)))))
