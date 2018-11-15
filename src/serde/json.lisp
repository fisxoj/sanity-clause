(defpackage sanity-clause.serde.json
  (:use #:cl
	#:alexandria
	#:sanity-clause.field)
  (:export #:lisp-name-to-json-name
	   #:json-name-to-lisp-name))

(in-package #:sanity-clause.serde.json)


(defun lisp-name-to-json-name (name)
  "Converts a kebab style name to a pascal case name. eg. ``some-field => someField``"

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
  "Converts a lower pascal case name to kebab case. eg. ``someField => some-field``"

  (with-output-to-string (s)
    (loop
      for c across name
      if (upper-case-p c)
	do (princ #\- s)
	and do (princ (char-downcase c) s)
      else
	do (princ c s))))

(defmethod sanity-clause.serde:dump ((schema list) value &optional (format (eql :json)))
  (jonthan:with-object
    (loop
      for (marker field) on schema by #'cddr
      for value = (getf data marker)
      for field-name = (or (sanity-clause.field:data-key-of field)
			   (string marker))
      for json-field = (if (symbolp field-name)
			   (lisp-name-to-json-name field-name)
			   field-name)

      when (sanity-clause.field:dump-field-p field)
	do (sanity-clause.field:validate field)
	and do (jojo:write-key-value (lisp-name-to-json-name (attribute field)) (get-value field value)))))
