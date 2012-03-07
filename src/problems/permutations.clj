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

(defn permutations-by-inject-recursive [^String s]
  (let [len (.length s)]
    (if (= 2 len)
      [s, (cs/reverse s)]
      (let [[head, tail] (break-str-at s 1)
            perms (permutations-by-inject-recursive tail)]
        (flatten (map #(inject-at-all-pos % head)
                      perms))))))

(defn permutations-by-inject-recursive-faster [^String s]
  (let [len (.length s)]
    (if (= 2 len)
      [s, (cs/reverse s)]
      (let [[head, tail] (break-str-at s 1)
            tail-perms (permutations-by-inject-recursive-faster tail)]
        (loop [tperms tail-perms
               all-perms []]
          (let [p (first tperms)]
            (if p
              (recur (next tperms)
                     (into all-perms
                           (inject-at-all-pos p head)))
              all-perms)))))))

(defn permutations-by-inject-iterative [^String s]
  (let [[head, tail] (break-str-at s 2)]
    (loop [perms [head, (cs/reverse head)]
           rem tail]
      (let [char (first rem)]
        (if char
          (recur (vec (flatten (map #(inject-at-all-pos % char) perms)))
                 (next rem))
          perms)))))

(defn permutations-by-inject-iterative-faster [^String s]
  (let [[head, tail] (break-str-at s 2)]
    (loop [perms [head, (cs/reverse head)]
           rem tail]
      (let [char (first rem)]
        (if char
          (let [new-perms (loop [acc []
                                 p perms]
                            (let [fp (first p)]
                              (if fp
                                (recur (into acc (inject-at-all-pos fp char)) (next p))
                                acc)))]
            (recur new-perms (next rem)))
          perms)))))

