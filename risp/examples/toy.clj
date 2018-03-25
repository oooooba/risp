; factorial(20) == 120
(= ((fn fact [n] (if (= n 0) 1 (* n (fact (- n 1))))) 5) 120)

(= (let [[[x1 y1][x2 y2]] [[1 2] [3 4]]] [x1 y1 x2 y2])
   [1 2 3 4])
