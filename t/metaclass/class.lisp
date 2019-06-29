(defpackage sanity-clause/test.metaclass.class
  (:use :cl :alexandria :rove))

(in-package :sanity-clause/test.metaclass.class)

;; (defclass poato ()
;;   ((name :type string
;;          :reader name-of
;;          :validator (:not-empty)
;;          :initarg :name)
;;    (variety :type (member :yam :idaho)
;;             :initarg :variety
;;             :reader variety-of
;;             :field (:member
;;                     :members (:yam :idaho))))
;;   (:metaclass sanity-clause.metaclass:validated-metaclass))


(defun dumb-list-eq (list1 list2)
  (every 'eq list1 list2))


(defun plist-eq (list1 list2)
  (and (= (length list1) (length list2))
       (let ((result t))
         (doplist (k v1 list1)
                  (unless
                      (let ((v2 (getf list2 k)))
                        (typecase v1
                          (cons (dumb-list-eq v1 v2))
                          (t (eq v1 v2))))
                    (setf result nil)))
         result)))


(deftest class-initargs
  (defclass orange ()
    ((seeds :initarg :seeds
            :initarg :seed-number)
     (cultivar :initarg :cultivar)))

  (ok (set-equal (sanity-clause.metaclass.class::class-initargs (c2mop:ensure-finalized (find-class 'orange))) '(:seeds :seed-number :cultivar))
      "collects the initargs of a given class."))


(deftest take-properties
  (multiple-value-bind (found others) (sanity-clause.metaclass.class::take-properties '(:p :r) '(:p 1 :c 2 :r 3 :q 5))

    (ok (dumb-list-eq found '(:p 1 :r 3))
        "takes the properties specified.")

    (ok (dumb-list-eq others '(:c 2 :q 5))
        "filters out the other properties.")))


(deftest test-merge-plist
  (let ((merged (sanity-clause.metaclass.class::merge-plist '(:a) '(:a (1 2) :b 3) '(:c 4 :a (3)))))

    (ok (= (length merged) 6)
        "merges lists.")

    (ok (every 'keywordp (loop for (k v) on merged by #'cddr collecting k))
        "preserves keys.")))


(deftest test-metaclass
  (testing "without any slots"
    (ok (defclass validated ()
          ()
          (:metaclass sanity-clause.metaclass.class:validated-metaclass))
        "can define a class with VALIDATED-METACLASS as the metaclass."))

  (testing "with slots that use type-derived field classes"
    (ok (defclass validated2 ()
          ((name :type string :default "larry"))
          (:metaclass sanity-clause.metaclass.class:validated-metaclass))
        "can define a class with a simple slot."))

  (testing "with slots that have explicit field types"
    (ok (defclass validated3 ()
          ((name :type string
                 :field-type :member
                 :members ("yam" "idaho")))
          (:metaclass sanity-clause.metaclass.class:validated-metaclass))
        "can define a class.")

    (c2mop:ensure-finalized (find-class 'validated3))

    (let ((name-field (sanity-clause.metaclass.class::field-of (find 'name (c2mop:class-direct-slots (find-class 'validated3))
                                                                     :key 'c2mop:slot-definition-name
                                                                     :test 'eq))))
      (ok (typep name-field 'sanity-clause.field:member-field)
          "the field is of the correct type."))))


(deftest test-environment
  (ok (defclass environment-sourced ()
        ((favorite-dog :type symbol
                       :field-type :member
                       :members (:wedge :walter)
                       :initarg :favorite-dog
                       :required t)
         (age :type integer
              :initarg :age
              :required t)
         (potato :type string
                 :initarg :potato
                 :required t))
        (:metaclass sanity-clause.metaclass.class:validated-metaclass))
      "can define the class.")

  (ok (make-instance 'environment-sourced :source :env)
      "can load from the envrionment."))


(deftest test-inheritance

  (testing "Redefining slots with the same name"
    (defclass b ()
      ((pie :type symbol
            :field-type :member
            :members (:apple :cherry)
            :initarg :pie))
      (:metaclass sanity-clause.metaclass.class:validated-metaclass))

    (defclass b ()
      ((pie :type string
            :field-type :member
            :members ("peach" "key-lime")
            :initarg :pie))
      (:metaclass sanity-clause.metaclass.class:validated-metaclass))

    (ok (signals (make-instance 'b :pie :apple) 'sanity-clause.field:conversion-error)
        "take the most specific definition of the field, raising an error for values that were valid for the old version.")

    (ok (make-instance 'b :pie "peach")
        "accept values for the new version of the slot.")))
