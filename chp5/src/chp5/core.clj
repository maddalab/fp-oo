(def apply-message-to
    (fn [type instance message & args]
        (let [method (or (message (:__instance_methods__ type)) message)]
            (apply method instance args)
        )
    )
)

(def make 
    (fn [type & args]
        (let [seeded {:__class_symbol__ (:__own_symbol__ type)}]
            (apply apply-message-to type seeded :add-instance-values args)
        )
    )
)

(def send-to
    (fn [instance message & args]
        (let [type (eval (:__class_symbol__ instance))]
            (apply apply-message-to type instance message args)
        )
    )
)

(def class-from-instance
     (fn [instance]
       (eval (:__class_symbol__ instance))))

(def Point
    {
        :__own_symbol__ 'Point
        :__instance_methods__ {
            :class-name :__class_symbol__
            :class (fn [this] (class-from-instance this))

            :add-instance-values (fn [this x y]
                (assoc this :x x :y y)
            )

            :shift (fn [this xinc yinc]
                (make Point (+ (:x this) xinc)
                            (+ (:y this) yinc))
            )
        }
    })

(def point (make Point 1 2))
(prn (make Point 1 2))
(prn (send-to (make Point 1 2) :shift 1 2))

(prn (send-to (make Point 1 2) :class-name))
(prn (send-to (make Point 1 2) :class))

;; exercise -3 redefine class

(def Point
    {
        :__own_symbol__ 'Point
        :__instance_methods__ {
            :class-name :__class_symbol__
            :class (fn [this] (class-from-instance this))

            :add-instance-values (fn [this x y]
                (assoc this :x x :y y)
            )

            :origin (fn [this] (make Point 0 0))

            :shift (fn [this xinc yinc]
                (make Point (+ (:x this) xinc)
                            (+ (:y this) yinc))
            )
        }
    })

(def Holder 
{
    :__own_symbol__ 'Holder
    :__instance_methods__ 
    {
        :add-instance-values (fn [this held]
            (assoc this :held held)
        )
    }
})