(def make (fn [type-fn & args]
  (apply type-fn args)  
))

(def send-to 
    (fn [obj mesg & args]
      (apply (mesg (:__methods__ obj)) obj args)))

(def Point
  (fn [x y]
    {
      :x x
      :y y

      :__class_symbol__ 'Point
      :__methods__ {
        :class :__class_symbol__

        :x :x
        :y :y
        :shift (fn [this xinc yinc]
                  (make Point (+ (send-to this :x) xinc) 
                              (+ (send-to this :y) yinc)))

        :add (fn [this other]
                  (send-to this :shift (send-to other :x) 
                                       (send-to other :y)))
      }
    }))


(def my-point (make Point 1 2))
(send-to my-point :x)
(send-to my-point :y)
(send-to my-point :shift -1 -100)
(send-to my-point :add (make Point -1 -100))
