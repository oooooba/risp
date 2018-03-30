(= (+ 1 2) 3)
(= (/ (- (+ 5 (* 2 3)) 3) 4) 2)

(def x 3)
(= x 3)
(def x 4)
(= x 4)
(def y (+ 1 7))
(= y 8)

(def mynum 111)
(def MYNUM 222)
(= mynum 111)
(= MYNUM 222)

(= (let [z (+ 2 3)] (+ 1 z)) 6)
(= (let [p (+ 2 3) q (+ 2 p)] (+ p q)) 12)
(def y (let [z 7] z))
(= y 7)

(def a 4)
(= (let [q 9] q) 9)
(= (let [q 9] a) 4)
(= (let [z 2] (let [q 9] a)) 4)
(let [x 4] (def a 5))
(= a 5) ; in mal test code, expected (= a 4)

(= (let [z 9] z) 9)
(= (let [a 5 b 6] [3 4 a [b 7] 8]) [3 4 5 [6 7] 8])
