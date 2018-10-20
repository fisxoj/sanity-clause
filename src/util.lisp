(defpackage sanity-clause.util
  (:use #:cl
	#:alexandria)
  (:export #:get-value))

(in-package #:sanity-clause.util)


(defun get-value (list key &optional default)
  "Generic reader function for {a,p}lists.  Returns a ``(values value found-p)`` so you can tell if the value was in the list or not."

  (etypecase list
    (trivial-types:property-list

     (values (getf list key default) (not (null (find key list)))))

    (trivial-types:association-list

     (let* ((assoc-cons (assoc key list))
	    (value (or (cdr assoc-cons) default)))
       (values value (not (null assoc-cons)))))))
