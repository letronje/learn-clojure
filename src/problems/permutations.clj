(ns problems.core
  (:require [clojure.string :as cs]))

(defn break-str-at [^String s, ^long n]
  [(.substring s 0 n)
   (.substring s n)])

(defn all-str-pieces [^String s]
  (map #(break-str-at s %)
       (range (inc (.length s)))))

(defn inject-at-all-pos [^String s, ^String c]
  (let [pieces (all-str-pieces s)]
    (map #(str (% 0) c (% 1))
         pieces)))

(defn permutations [^String s]
  (let [len (.length s)]
    (if (= 2 len)
      [s, (cs/reverse s)]
      (let [[head, tail] (break-str-at s 1)
            perms (permutations tail)]
        (flatten (map #(inject-at-all-pos % head)
                      perms))))))

(defn faster-permutations [^String s]
  (let [len (.length s)]
    (if (= 2 len)
      [s, (cs/reverse s)]
      (let [[head, tail] (break-str-at s 1)
            tail-perms (faster-permutations tail)]
        (loop [tperms tail-perms
               all-perms []]
          (let [p (first tperms)]
            (if p
              (recur (next tperms)
                     (into all-perms
                           (inject-at-all-pos p head)))
              all-perms)))))))
