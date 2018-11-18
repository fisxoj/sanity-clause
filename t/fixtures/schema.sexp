(:port (:integer :validator ((:int :min 0) (:int :max 5)))
 :name (:string :validator (:not-empty))
 :mode (:member :members (:on :off))
 :boolean (:boolean :default t))
