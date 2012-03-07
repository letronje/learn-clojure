(ns problems.core
  (:require [clojure.test :as ct]) )

(load "permutations")

(defn -main [& args]
  (let [s "abcdefghi"
        l (.length s)
        expected-perms (reduce * (range 1 (inc l))) ]
    (time  (println (count  (permutations-by-inject-recursive s))))
    (time  (println (count  (permutations-by-inject-recursive-faster s))))
    (time  (println (count  (permutations-by-inject-iterative s))))
    (time  (println (count  (permutations-by-inject-iterative-faster s))))))