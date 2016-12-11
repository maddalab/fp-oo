(def apply-message-to
    (fn [type instance message & args]
        (let [method (or (message (:__instance_methods__ type)) message)]
            (apply method instance args))))

(def make 
    (fn [type & args]
        (let [seeded {:__class_symbol__ (:__own_symbol__ type)}]
            (apply apply-message-to type seeded :add-instance-values args))))

(def send-to
    (fn [instance message & args]
        (let [type (eval (:__class_symbol__ instance))]
            (apply apply-message-to type instance message args))))

(def class-from-instance
     (fn [instance]
       (assert (map? instance))
       (eval (:__class_symbol__ instance))))

(def class-symbol-above (fn [type-symbol]
    (:__superclass_symbol__ (eval type-symbol))))

(def lineage-1 (fn [type-symbol sol]
    (if (nil? type-symbol)
        sol
        (recur
            (class-symbol-above type-symbol) 
            (cons type-symbol sol)))))

(def lineage (fn [type-symbol] (lineage-1 type-symbol [])))

(def class-instance-methods (fn [type-symbol]
    (:__instance_methods__ (eval type-symbol))))

(def method-cache (fn [type]
    (let [type-symbol (:__own_symbol__ type)
          method-maps (map class-instance-methods (lineage type-symbol))]
          (apply merge method-maps))))

(def apply-message-to (fn [type instance message & args]
    (let [method (message (method-cache type))]
        (apply method instance args))))

(def RedPoint {
    :__own_symbol__ 'RedPoint
    :__superclass_symbol__ 'Point
    :__instance_methods__ {
        :color (fn [this] "red")
    }
})

(def Point {
    :__own_symbol__ 'Point
    :__superclass_symbol__ 'Anything
    :__instance_methods__ {
        :add-instance-values (fn [this x y]
            (assoc this :x x :y y)
        )

        :shift (fn [this xinc yinc]
            (make Point (+ (:x this) xinc)
                        (+ (:y this) yinc))
        )
    }
})

(def Anything {
    :__own_symbol__ 'Anything

    :__instance_methods__ {
        :class-name :__class_symbol__
        :class (fn [this] (class-from-instance this))

        :add-instance-values identity
    }
})

(def factorial (fn [n]
    (if (or (= n 0) (= n 1))
        1
        (* n (factorial (- n 1))))))

(def factorial-1 (fn [n so-far] 
    (if (or (= n 0) (= n 1))
        so-far
        (factorial-1 (- n 1) (* so-far n)))))

(def sum (fn [seq curr]
    (if (empty? seq)
        curr
        (sum (rest seq) (+ (first seq) curr)))))

(def multiply (fn [seq curr]
    (if (empty? seq)
        curr
        (multiply (rest seq) (* (first seq) curr)))))

(def recursive-function (fn [fun something so-far]
    (if (empty? something)
        so-far
        (recursive-function fun (rest something)
                            (fun (first something) so-far)))))

(def sum-1 (fn [seq]
    (recursive-function + seq 0)))

(def multiply-1 (fn [seq]
    (recursive-function * seq 1)))

(pprint (recursive-function (fn [sym hash] (assoc hash sym 0)) [:a :b :c] {}))

(pprint (recursive-function (fn [sym hash] (assoc hash sym (count hash))) [:a :b :c] {}))

