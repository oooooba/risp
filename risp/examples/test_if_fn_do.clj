(= ((fn [a b] (+ b a)) 3 4) 7)
(= ((fn [] 4)) 4)
(= ((fn [f x] (f x)) (fn [a] (+ 1 a)) 7) 8)
(= (((fn [a] (fn [b] (+ a b))) 5)  7) 12)
(= (let [gen-plus5 (fn [] (fn [b] (+ 5 b)))]
     (let [plus5 (gen-plus5)]
       (plus5 7)))
   12)
(= (let [gen-plusX (fn [x] (fn [b] (+ x b)))]
     (let [plus7 (gen-plusX 7)]
       (plus7 8)))
   15)
