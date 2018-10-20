(defpackage t.sanity-clause.util
  (:use #:cl
	#:alexandria
	#:prove
	#:sanity-clause.util))

(in-package #:t.sanity-clause.util)

(plan 1)

(subtest "get-value"
  (let ((alist '((:value . 2)))
	(plist '(:value 2)))

    (subtest "alist"
      (multiple-value-bind (value found-p) (get-value alist :value)
	(is value 2
	    "Finds a value that exists.")
	(is found-p t
	    "Says it found the value for a value that exists."))

      (multiple-value-bind (value found-p) (get-value alist :nonexistent :default)
	(is value :default
	    "Returns the default value for a missing value.")
	(is found-p nil
	    "Knows it didn't find the value for a missing value.")))

    (subtest "plist"
      (multiple-value-bind (value found-p) (get-value plist :value)
	(is value 2
	    "Finds a value that exists.")
	(is found-p t
	    "Says it found the value for a value that exists."))

      (multiple-value-bind (value found-p) (get-value plist :nonexistent :default)
	(is value :default
	    "Returns the default value for a missing value.")
	(is found-p nil
	    "Knows it didn't find the value for a missing value.")))))

(finalize)
