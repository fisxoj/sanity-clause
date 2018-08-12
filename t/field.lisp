(defpackage t.sanity-clause.field
  (:use #:cl
	#:prove
        #:alexandria
        #:cl-arrows))

(in-package #:t.sanity-clause.field)

(plan 2)

(defclass inventory ()
  ((potato-count :reader potato-count-of :initarg :count)))

(subtest "get-value"
  (subtest "missing values"
    (let ((default-field (make-instance 'sanity-clause.field:field :default 3 :attribute 'potato-count))
	  (field (make-instance 'sanity-clause.field:field :attribute 'potato-count))
	  (clos-inventory (make-instance 'inventory))
	  (plist-inventory '())
	  (alist-inventory))

      (is (sanity-clause.field::get-value default-field clos-inventory) 3
	  "Default values work for clos objects.")

      (is (sanity-clause.field::get-value field clos-inventory) :missing
	  "A missing value with no default is :missing for clos objects.")

      (is (sanity-clause.field::get-value default-field plist-inventory) 3
	  "Default values work for plists.")

      (is (sanity-clause.field::get-value field plist-inventory) :missing
	  "A missing value with no default is :missing for plists.")

      (is (sanity-clause.field::get-value default-field alist-inventory) 3
	  "Default values work for alists.")

      (is (sanity-clause.field::get-value field alist-inventory) :missing
	  "A missing value with no default is :missing for alists."))))

(subtest "validate"
  (let ((int-field (make-instance 'sanity-clause.field:field :validator #'v:int))
        (email-field (make-instance 'sanity-clause.field:field :validator '(v:str v:email))))

    (is (sanity-clause.field::validate int-field "4") nil
        "Acceptable values should not raise validation-error.")

    (is-error (sanity-clause.field::validate int-field "balloon") 'sanity-clause.field:validation-error
              "garbage strings raise errors get marshalled to integers.")

    (handler-case
        (progn
          (sanity-clause.field::validate email-field 4)
          (fail "Failed to raise validation error when multiple validators should fail."))

      (sanity-clause.field:validation-error (e)
        (is (length (sanity-clause.field::error-messages-of e)) 2
            "Got two failed validations when they should have failed.")))))

(finalize)
