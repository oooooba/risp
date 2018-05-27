(def list (fn [& l] l))

(def get (fn [obj key] (_get obj key nil)))

(def vector (fn [& xs] (_vector xs)))

(def vec (fn [coll] (_vec coll)))

(def boolean (fn [x] (_boolean x)))

(def not (fn [x] (if (boolean x) false true)))
