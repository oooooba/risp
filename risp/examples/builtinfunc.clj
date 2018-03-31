(= (cons 1 (quote (2 3 4 5 6))) (quote (1 2 3 4 5 6)))
(= (cons 1 '(2 3 4 5 6)) '(1 2 3 4 5 6))

(= (list? '(1 2 3)) true)
(= (list? (list 1 2)) true)
(= (list? 0) false)
(= (list? {}) false)
(= (list? []) false)
