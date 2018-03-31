(= (list) ())
(= (list? (list)) true)

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

(def gen-plus5 (fn [] (fn [b] (+ 5 b))))
(def plus5 (gen-plus5))
(= (plus5 7) 12)
(def gen-plusX (fn [x] (fn [b] (+ x b))))
(def plus5 (gen-plusX 5))
(= (plus5 8) 13)

(= (do) nil)
(= (do (println "println output1")) nil)
(= (do (println "println output2") 7) 7)
(= (do (println "println output3") (println "println output4") (+ 1 2)) 3)
