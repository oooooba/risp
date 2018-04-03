(= (list) ())
(= (list? (list)) true)

(= (if true 7 8) 7)
(= (if false 7 8) 8)
(= (if true (+ 1 7) (+ 1 8)) 8)
(= (if false (+ 1 7) (+ 1 8)) 9)
(= (if nil 7 8) 8)
(= (if 0 7 8) 7)
(= (if "" 7 8) 7)
(= (if (list) 7 8) 7)
(= (if (list 1 2 3) 7 8) 7)
(= (= (list) nil) false)

(= (if false (+ 1 7)) nil)
(= (if nil 8 7) 7)
(= (if true (+ 1 7)) 8)

(= (= 2 1) false)
(= (= 1 1) true)
(= (= 1 2) false)
(= (= 1 (+ 1 1)) false)
(= (= 2 (+ 1 1)) true)
(= (= nil 1) false)
(= (= nil nil) true)

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
