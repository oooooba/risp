(def not
  (fn [x]
      (if (= x false)
        true
        (if (= x nil)
          true
          false))))
