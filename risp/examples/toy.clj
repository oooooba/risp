; factorial(20) == 120
(= ((fn fact [n] (if (= n 0) 1 (* n (fact (- n 1))))) 5) 120)

(= (let [[[x1 y1][x2 y2]] [[1 2] [3 4]]] [x1 y1 x2 y2])
   [1 2 3 4])
(= (let [[x1 y1] [1 2], [x2 y2] [3 4]] [x1 y1 x2 y2])
   [1 2 3 4])
(= (let [[[x1 y1][x2 y2] & z & [w1 w2] :as v] [[1 2] [3 4] 5 6]] [x1 y1 x2 y2 z w1 w2 v])
   [1 2 3 4 [5 6] 5 6 [[1 2] [3 4] 5 6]])
(= (let [{a :a, b :b, c :c, :as m :or {a 2 b 3}} {:a 5 :c 6}] [a b c m])
   [5 3 6 {:c 6, :a 5}])
(= (let [m {:j 15, :k 16, :ivec [22 23 24 25]},
         {j :j, k :k, i :i, [r s & t :as v] :ivec, :or {i 12, j 13}} m]
        [i j k r s t v])
   [12 15 16 22 23 [24 25] [22 23 24 25]])
