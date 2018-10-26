(defpackage sanity-clause.util
  (:use #:cl
	#:alexandria)
  (:export #:get-value))

(in-package #:sanity-clause.util)


(defun get-value (object key &optional default)
  "Generic reader function for lists and class instances.  Returns a ``(values value found-p)`` so you can tell if the value was in the list or not."

  (etypecase object
    (standard-object
     (if (and (slot-exists-p object key) (slot-boundp object key))
         (values (slot-value object key) t)
         (values default nil)))

    (trivial-types:property-list

     (values (getf object key default) (not (null (find key object)))))

    (trivial-types:association-list

     (let* ((assoc-cons (assoc key object))
	    (value (or (cdr assoc-cons) default)))
       (values value (not (null assoc-cons)))))))
