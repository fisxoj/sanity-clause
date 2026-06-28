(in-package :sanity-clause-2/serde)


(defun kebab-to-shouting-snake (string)
  (with-output-to-string (out)
    (loop :for char :across string
          :do (write-char (if (char= char #\-) #\_ (char-upcase char)) out))))


(defun shouting-snake-to-kebab (string)
  (with-output-to-string (out)
    (loop :for char :across string
          :do (write-char (if (char= char #\_) #\- (char-downcase char)) out))))


(defclass environment ()
  ())


(defmethod serialize ((serde environment) instance)
  )


(defmethod deserialize-value ((serde environment) (field sanity-clause-2::boolean-field) (value string))
  (cond
    ((member value '("1" "y" "yes" "t" "true"  "on"  "enable" ) :test #'string-equal) t)
    ((member value '("0" "n" "no"  "f" "false" "off" "disable") :test #'string-equal) nil)
    (t (error "couldn't convert ~a to a boolean." value))))


(defmethod deserialize-value ((serde environment) (field sanity-clause-2::integer-field) (value string))
  (parse-integer value))


(defmethod slot-key ((serde environment) (slot sanity-clause-2::validated-slot-mixin))
  (or (call-next-method)
      (kebab-to-shouting-snake (string (closer-mop:slot-definition-name slot)))))


(defmethod deserialize ((serde environment) class)
  (let* ((instance (make-instance class))
         (class (if (typep class 'symbol) (find-class class) class))
         (separator (get-serde-option class :environment :separator)))

    (loop :for slot :in (closer-mop:class-slots class)
          :for field := (sanity-clause-2::field slot)
          :for slot-name := (closer-mop:slot-definition-name slot)

          :when  (sanity-clause-2:deserialize-p field)
            :if (typep field 'sanity-clause-2::forward-reference-field)
              :do (sanity-clause-2::with-path ((slot-key serde slot))
                    (setf (closer-mop:slot-value-using-class class instance slot)
                          (deserialize-value serde field (deserialize serde (closer-mop:slot-definition-type slot)))))
          :else
            :do (let* ((env-var (str:join separator (append (sanity-clause-2::current-path) (list (slot-key serde slot)))))
                       (value (uiop:getenv env-var)))
                  (setf (closer-mop:slot-value-using-class class instance slot) (deserialize-value serde field value))))

    (values instance)))
