(defpackage t.sanity-clause.serde
  (:use #:cl
	#:prove
	#:alexandria
	#:sanity-clause.serde)
  (:shadowing-import-from #:sanity-clause.serde
			  #:load))

(in-package #:t.sanity-clause.serde)

(plan 1)

(subtest "load"
  (subtest "plist"
    (let ((plist-schema (list :name (make-instance 'sanity-clause.field:field
						   :validator (lambda (v) (v:str v :min-length 3)))
			      :age  (make-instance 'sanity-clause.field:integer-field)))
	  (acceptable-data '(:name "Matt" :age 30))
	  (bad-age-data    '(:name "Matt" :age "potato"))
	  (bad-name-data   '(:name 11     :age 30)))

      (ok (load plist-schema acceptable-data)
	  "Can load from valid data")
      (is-error (load plist-schema bad-age-data) 'sanity-clause.field:validation-error)
      )))

(finalize)
