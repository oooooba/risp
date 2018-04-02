(def not
  (fn [x]
      (if (= x false)
        true
        (if (= x nil)
          true
          false))))

(def list
  (fn [& l] l))

(def get (fn [obj key] (_get obj key nil)))
