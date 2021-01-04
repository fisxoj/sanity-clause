(defpackage sanity-clause.util
  (:use #:cl
	#:arrows
	#:alexandria)
  (:export #:get-value
           #:do-key-values))

(in-package #:sanity-clause.util)

(defun lisp-name->env-name (lisp-name)
  "Convert kebab case to upper snake case, e.g.
``some-name => SOME_NAME``"

  (->> lisp-name
       string-upcase
       (str:replace-all "-" "_")))


(defun get-value (object key &optional default)
  "Generic reader function for lists and class instances.  Returns a ``(values value found-p)`` so you can tell if the value was in the list or not.  Can also read from the environment if given ``:env`` as OBJECT."

  (if (and (keywordp object) (eq object :env))
      ;; if key is a string, assume it's already an environment variable name like MY_VARIABLE,
      ;; if a symbol, assume we have to convert from kebab case to upper snake case.
      (let ((env-variable-name (etypecase key
				 (string key)
				 (symbol (lisp-name->env-name key)))))
	(values (or (uiop:getenvp env-variable-name) default) (not (not (uiop:getenvp env-variable-name)))))

      (etypecase object
	(standard-object
	 (if (and (slot-exists-p object key) (slot-boundp object key))
	     (values (slot-value object key) t)
	     (values default nil)))

	(trivial-types:property-list

	 (values (getf object key default) (not (null (find key object)))))

	(trivial-types:association-list

	 (let* ((assoc-cons (assoc key object :test #'string-equal))
		(value (or (cdr assoc-cons) default)))
	   (values value (not (null assoc-cons)))))

        (hash-table

         (gethash key object default)))))


(defmacro do-key-values ((key value) data &body body)
  (with-gensyms (alist)
    `(let ((,alist (etypecase ,data
                     (hash-table
                      (hash-table-alist ,data))
                     (trivial-types:property-list
                      (plist-alist ,data))
                     (trivial-types:association-list
                      ,data))))
       (loop for (,key . ,value) in ,alist
             do (progn ,@body)))))
