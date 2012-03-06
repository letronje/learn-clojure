(ns problems.core
  (:require [clojure.test :as ct]) )

(load "permutations")

(defn -main [& args]
  (let [s "abcdefghi"
        l (.length s)
        expected-perms (reduce * (range 1 (inc l))) ]
    (time  (println (count  (permutations s))))
    (time  (println (count  (faster-permutations s))))))