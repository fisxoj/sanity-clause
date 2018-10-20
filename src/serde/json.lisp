(defpackage sanity-clause.serde.json
  (:use #:cl
	#:alexandria
	#:sanity-clause.field
	#:sanity-clause.schema)
  (:export #:lisp-name-to-json-name
	   #:json-name-to-lisp-name))

(in-package #:sanity-clause.serde.json)

(defun lisp-name-to-json-name (name)
  (with-output-to-string (s)
    (loop
      for c across name
      with last-char-dash = nil
      if (char= c #\-)
	do (setf last-char-dash t)
      else
	do (princ (if last-char-dash (char-upcase c) c) s)
	and do (setf last-char-dash nil))))

(defun json-name-to-lisp-name (name)
  (with-output-to-string (s)
    (loop
      for c across name
      if (upper-case-p c)
	do (princ #\- s)
	and do (princ (char-downcase c) s)
      else
	do (princ c s))))

(defclass json-serializer ()
  ())

;; (defmethod serialize ((schema schema) (serializer json-serializer) value)
;;   (jojo:with-object
;;     (dolist (field (schema-fields-of schema))
;;       (serialize field value))))

(defmethod serialize ((field field) (serializer json-serializer) value)
  (jojo:write-key-value (lisp-name-to-json-name (attribute field)) (get-value field value)))
