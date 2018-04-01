(= (try) nil)
(= (try (catch Symbol name)) nil)
(= (try (finally)) nil)
(= (try (catch Symbol name) (finally)) nil)
(= (try (catch Symbol name) (catch Symbol name) (finally)) nil)

(= (try ()) ())
(= (try () (catch Symbol name 2)) ())
(= (try () (finally 3)) ())
(= (try () (catch Symbol name 2) (finally 3)) ())
(= (try () (catch Symbol name 2) (catch Symbol name 2) (finally 3)) ())

(= (try () 1) 1)
(= (try a (catch Symbol name (nil? name))) true)
(= (try a (finally "b" :c)) nil)
(= (try a (catch Symbol name (nil? name)) (finally "b" :c)) true)
(= (try a (catch Symbol name (nil? name)) (catch Ignored name (a) [4]) (finally "b" :c)) true)
