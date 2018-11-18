(defpackage sanity-clause/test.util
  (:use #:cl
	#:alexandria
	#:rove
	#:sanity-clause.util))

(in-package #:sanity-clause/test.util)


(defclass test-class ()
  ((value :initarg :value)))


(deftest test-get-value
  (let ((alist '((:value . 2)))
	(plist '(:value 2))
        (object (make-instance 'test-class :value 2)))

    (testing "alists"
      (multiple-value-bind (value found-p) (get-value alist :value)
	(ok (= value 2)
	    "Finds a value that exists.")
	(ok (eq found-p t)
	    "Says it found the value for a value that exists."))

      (multiple-value-bind (value found-p) (get-value alist :nonexistent :default)
	(ok (eq value :default)
	    "Returns the default value for a missing value.")
	(ok (eq found-p nil)
	    "Knows it didn't find the value for a missing value.")))

    (testing "plists"
      (multiple-value-bind (value found-p) (get-value plist :value)
	(ok (= value 2)
	    "Finds a value that exists.")
	(ok (eq found-p t)
	    "Says it found the value for a value that exists."))

      (multiple-value-bind (value found-p) (get-value plist :nonexistent :default)
	(ok (eq value :default)
	    "Returns the default value for a missing value.")
	(ok (eq found-p nil)
	    "Knows it didn't find the value for a missing value.")))

    (testing "objects"
      (multiple-value-bind (value found-p) (get-value object 'value)
        (ok (= value 2)
            "finds a value that exists.")
        (ok (eq found-p t)
            "says it found the value for a value that exists."))

      (multiple-value-bind (value found-p) (get-value object 'nonexistent :default)
        (ok (eq value :default)
            "returns the default value for a missing value.")
        (ok (eq found-p nil)
            "knows it didn't find a value for a missing value.")))

    (testing "environment variables"
      (multiple-value-bind (value found-p) (get-value :env 'value)
        (ok (string= value "2")
            "finds a value that exists.")
        (ok (eq found-p t)
            "says it found the value for a value that exists."))

      (multiple-value-bind (value found-p) (get-value object :env :default)
        (ok (eq value :default)
            "returns the default value for a missing value.")
        (ok (eq found-p nil)
            "knows it didn't find a value for a missing value.")))))
