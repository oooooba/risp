; factorial(20) == 120
(= ((fn fact [n] (if (= n 0) 1 (* n (fact (- n 1))))) 5) 120)
