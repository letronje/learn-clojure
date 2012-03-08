(ns problems.core
  (:require [clojure.test :as ct]) )

(load "permutations")

(defn -main [& args]
  (let [s "abcdefghi"
        l (.length s)
        expected-perms (reduce * (range 1 (inc l))) ]
    (dotimes [n 10]
      (println)
      (println (str  "#" n))
      (-> s permutations-by-inject-recursive count println time)
      (-> s permutations-by-inject-iterative count println time)
      (-> s permutations-by-swap-iterative count println time)
      )))
