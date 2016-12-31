;; This imports a function from another namespace. (Think package or module.)
(use '[clojure.pprint :only [cl-format]])

(def class-instance-methods (fn [type-symbol]
    (:__instance_methods__ (eval type-symbol))))

(def class-symbol-above (fn [type-symbol]
    (:__superclass_symbol__ (eval type-symbol))))

(def lineage-1 (fn [type-symbol sol]
    (if (nil? type-symbol)
        sol
        (recur
            (class-symbol-above type-symbol) 
            (cons type-symbol sol)))))

(def lineage (fn [type-symbol] (lineage-1 type-symbol [])))

(def method-cache (fn [type]
    (let [type-symbol (:__own_symbol__ type)
          method-maps (map class-instance-methods (lineage type-symbol))]
          (apply merge method-maps))))

(def apply-message-to (fn [type instance message args]
    (let [mcache (method-cache type)
            method (message mcache)
            missing (:method-missing mcache)]
        (if (nil? method)
            (missing instance message args)
            (apply method instance args)))))

(def send-to
    (fn [instance message & args]
        (let [type (eval (:__class_symbol__ instance))]
            (apply-message-to type instance message args))))

(def class-from-instance
     (fn [instance]
       (assert (map? instance))
       (eval (:__class_symbol__ instance))))

(def MetaAnything {
  :__own_symbol__ 'MetaAnything
  :__superclass_symbol__ 'Anything
  :__class_symbol__ 'Anything
  :__instance_methods__ {
    :new (fn [type & args]
      (let [seeded {:__class_symbol__ (:__own_symbol__ type)}]
        (apply-message-to type seeded :add-instance-values args)))
  }
})

(def Anything {
    :__own_symbol__ 'Anything
    :__class_symbol__ 'MetaAnything

    :__instance_methods__ {
        :class (fn [this] (class-from-instance this))
        :class-name :__class_symbol__
        :to-string (fn [this] (str this))

        :add-instance-values identity

        :method-missing (fn [this message args]
            (throw (Error. (cl-format nil "A ~A does not accept the message ~A."
                                (send-to this :class-name)
                                message))))
    }
})

(def MetaPoint {
  :__own_symbol__ 'MetaPoint
  :__superclass_symbol__ 'MetaAnything
  :__class_symbol__ 'Anything
  :__instance_methods__ {
    :origin (fn [type] (send-to type :new 0 0))
  }
})


(def Point {
    :__own_symbol__ 'Point
    :__class_symbol__ 'MetaPoint
    :__superclass_symbol__ 'Anything
    :__instance_methods__ {
        :add-instance-values (fn [this x y]
            (assoc this :x x :y y))

        :x :x
        :y :y

        :shift (fn [this xinc yinc]
            (send-to Point :new (+ (:x this) xinc)
                        (+ (:y this) yinc)))

        :add (fn [this other]
            (send-to this :shift (:x other) (:y other)))

        :to-string (fn [this]
            (cl-format nil "A ~A like this: [~A, ~A]"
                (send-to this :class-name)
                (send-to this :x)
                (send-to this :y)))
    }
})

(def MetaMissingOverrider {
    :__own_symbol__ 'MetaMissingOverrider
    :__superclass_symbol__ 'MetaAnything

    :__instance_methods__ {}
})

(def MissingOverrider {
    :__own_symbol__ 'MissingOverrider
    :__class_symbol__ 'MetaMissingOverrider
    :__superclass_symbol__ 'Anything

    :__instance_methods__ {
        :method-missing (fn [this message args]
            (println (cl-format nil "method-missing called! ~A on ~A." message this))
            (println (cl-format nil "The arguments were ~A." args))
        )
    }
})