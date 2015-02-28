(ns macro-test.core)

(comment
  (defmacro match [elem & bindings]
    `(foobar ~@bindings))

  (match-vector [:foo :bar] [:foo :bar])
  (match-vector [:foo '_] [:foo :bar])
  (match-vector [:foob '_] [:foo :bar])

  (match-elem [:foo '_] [:foo :bar])
  (match-elem [:foo '_] [:foob :bar])
  (match->if 'a '(['_ :foo] (+ 1 2) :else (+ 2 2)))
  (def a [:foo :bar])
  (match a
         [:fob _] (do (println "fob") (+ 1 2))
         [:foo _] (do (println "foo") (+ 2 2))
         :else (+ 3 2))
  (if (match-elem [_ :foo] a) (+ 1 2) (if (match-elem :else a) (+ 2 2)))
  select-keys

  clojure.core
  (escape ['_ :foo])
  (escape :else)
(seq? ['_ :foo])

  )

(defn escape [clause]
  (if (vector? clause)
    (mapv (fn [x] (if (= x '_) :_ x)) clause)
    clause))

(defn bit-match [[ clausebit elebit]] (or (= clausebit elebit ) (= clausebit :_)))
(defn pair-up [clause ele] (map (fn [a b] [a b]) clause ele))
(defn match-vector [clause ele]
  (every? bit-match (pair-up clause ele)))
(defn match-elem [clause element]
  (cond (= clause :else) true
        (vector? element) (match-vector clause element )
        :else (= clause element)))

(defmacro match [elem & bindings]
  (match->if elem bindings))

(defn match->if [elem [clause value & r]]
  (if (nil? r)
    (list 'if (list 'match-elem (escape clause) elem) value)
    (list 'if (list 'match-elem (escape clause) elem) value
         (match->if elem r))))

(macroexpand '(match a
        [_ :foo] (+ 1 2)
        :else (+ 2 2)))



(defmacro code-critic
  "phrases are courtesy Hermes Conrad from Futurama"
  [{:keys [good bad]}]
  (list 'do
        (list 'println
              "Great squid of Madrid, this is bad code:"
              (list 'quote bad))
        (list 'println
              "Sweet gorilla of Manila, this is good code:"
              (list 'quote good))))

(code-critic {:good (+ 1 1) :bad (1 + 1)})
(macroexpand '(code-critic {:good (+ 1 1) :bad (1 + 1)}))


(defmacro postfix-notation
  [expression]
  (conj (butlast expression) (last expression)))

(macroexpand '(postfix-notation (1 1 +)))

(postfix-notation (1 1 +))
(conj '(2 3) 1)
