(defpackage sanity-clause.validator
  (:use #:cl
        #:arrows
	#:alexandria)
  (:shadow #:real)
  (:export #:str
	   #:int
	   #:real
	   #:email
	   #:uuid
	   #:not-empty

           #:hydrate-validators
           #:ensure-validator)
  (:documentation "Some validation functions that can be used with fields to make sure data has certain properties."))

(in-package :sanity-clause.validator)


(defun ensure-validator (keyword-spec)
  "Find a validator function by keyword-spec and return the function represented by the spec unless it's already a function."

  (if (functionp keyword-spec)
      keyword-spec
      (let ((keyword-spec (ensure-list keyword-spec))
            (validator-package (find-package :sanity-clause.validator)))

        (lambda (value)
          (apply (find-symbol (string-upcase (car keyword-spec)) validator-package)
                 value
                 (cdr keyword-spec))))))


(defun hydrate-validators (spec-plist)
  "Takes a list of validators in the form of keywords and turns them into living function references."

  (when-let ((validator-spec (getf spec-plist :validator)))
    (setf (getf spec-plist :validator) (mapcar #'ensure-validator validator-spec)))
  spec-plist)


(defmacro fail (value reason &rest format-args)
  `(format nil ,(format nil "Value ~~a ~S" reason) ,value ,@format-args))


(defun int (value &key (radix 10) max min)
  "Accepts a :param:`value` as an integer or string.
If :param:`value` is a string, interpret it as a value with base- :param:`radix`.
If specified, make sure :param:`min` <= :param:`value` <= :param:`max`."

  (handler-case
      (let ((number (etypecase value
                      (string (parse-integer value :radix radix))
                      (integer value))))

        (cond
          ((and max (> number max))
           (fail value "must be larger smaller than ~a" max))

          ((and min (< number min))
           (fail value "must be larger than ~a" min))

          (t nil)))

    (parse-error (e)
      (declare (ignore e))
      (fail value "must be parseable as an integer"))))


(defun bool (value)
  "Attempts to convert :param:`value` to a ``t`` or ``nil`` by comparing it to a list of stringy true or false values.  Throws a :class:`<validation-error>` if it can't figure out if :param:`value` is truthy or falsey."

  (cond
    ((member value '("y" "yes" "t" "true"  "on"  "enable" ) :test #'string-equal) nil)
    ((member value '("n" "no"  "f" "false" "off" "disable") :test #'string-equal) nil)
    (t (fail value "is not a valid boolean value"))))


(defun str (value &key min-length max-length)
  "Checks that :param:`value` is a string.
If specified, checks that :param:`min-length` <= ``(length value)`` <= :param:`max-length`."

  (cond
    ((not (typep value 'string))
     (fail value "must be a string."))

    ((and min-length
          (< (length value) min-length))
     (fail value "string length must be > ~d" min-length))

    ((and max-length
          (> (length value) max-length))
     (fail value "string length must be < ~d" max-length))

    (t nil)))



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
