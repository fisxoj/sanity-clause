(defpackage sanity-clause.validator
  (:use #:cl
        #:cl-arrows
	#:alexandria)
  (:shadow #:real)
  (:export #:str
	   #:int
	   #:real
	   #:email
	   #:uuid
	   #:not-empty

           #:hydrate-validators
           #:make-validator)
  (:documentation "Some validation functions that can be used with fields to make sure data has certain properties."))

(in-package :sanity-clause.validator)


(defun make-validator (keyword-spec)
  "Find a validator function by keyword-spec and return the function represented by the spec."

  (let ((keyword-spec (ensure-list keyword-spec)))

    (lambda (value) (apply (find-symbol (string-upcase (car keyword-spec)) (find-package :sanity-clause.validator)) value (cdr keyword-spec)))))


(defun hydrate-validators (spec-plist)
  "Takes a list of validators in the form of keywords and turns them into living function references."

  (when-let ((validator-spec (getf spec-plist :validator)))
    (setf (getf spec-plist :validator) (mapcar #'make-validator validator-spec)))
  spec-plist)


(defmacro convert-validate-validator (&body body)
  "Handles the errors thrown by a validator function from the :package:`validate` package to work with :package:`sanity-clause`."

  ;; Collect nils for non-errors, strings for errors.
  `(handler-case (prog1 nil
		   ,@body)
     (v:<validation-error> (e)
       (princ-to-string e))))


(defun int (value &key max min)
  "Checks that a number is an integer, optionally checking it's in the inclusive range [MIN, MAX]."

  (convert-validate-validator
    (v:int value :max max
		 :min min)))


(defun str (value &key min-length max-length)
  "Checks that the input is a string, optionally of a length between MIN-LENGTH and MAX-LENGTH."
  (convert-validate-validator
    (v:str value :min-length min-length
		 :max-length max-length)))


(defun email (value)
  "Checks that the input resembles an email."

  (unless (ppcre:scan ".+@.+\\\..{2,}" value)
    "Not a valid email address."))


(let ((uuid-scanner (ppcre:create-scanner '(:sequence
					    :start-anchor
					    (:greedy-repetition 8 8 (:char-class (:range #\0 #\9) (:range #\a #\f))) #\-
					    (:greedy-repetition 4 4 (:char-class (:range #\0 #\9) (:range #\a #\f))) #\-
					    (:greedy-repetition 4 4 (:char-class (:range #\0 #\9) (:range #\a #\f))) #\-
					    (:greedy-repetition 4 4 (:char-class (:range #\0 #\9) (:range #\a #\f))) #\-
					    (:greedy-repetition 12 12 (:char-class (:range #\0 #\9) (:range #\a #\f)))
					    :end-anchor)
					  :case-insensitive-mode t)))
  (defun uuid (value)
    "Checks that a value is a string that resembles a uuid."

    (unless (ppcre:scan uuid-scanner value)
      "Is not a valid uuid.")))


(defun not-empty (value)
  "Checks that some value has been supplied.  Note: this won't work well with boolean values because NIL is falsey."

  (when (emptyp value)
    "Must not be empty."))
