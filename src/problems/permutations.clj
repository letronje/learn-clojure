(ns problems.core
  (:require [clojure.string :as cs]))

(defn- break-str-at [^String s, ^long n]
  [(.substring s 0 n)
   (.substring s n)])

(defn- all-str-pieces [^String s]
  (map #(break-str-at s %)
       (range (inc (.length s)))))

(defn- inject-at-all-pos [^String s, ^String c]
  (let [pieces (all-str-pieces s)]
    (map #(str (% 0) c (% 1))
         pieces)))

(defn permutations-by-inject-recursive [^String s]
  (let [len (.length s)]
    (if (= 2 len)
      [s, (cs/reverse s)]
      (let [[head, tail] (break-str-at s 1)
            perms (permutations-by-inject-recursive tail)]
        (reduce #(into %1 %2) []
                (map #(inject-at-all-pos % head)
                     perms))))))

(defn permutations-by-inject-iterative [^String s]
  (let [[head, tail] (break-str-at s 2)]
    (loop [perms [head, (cs/reverse head)]
           rem tail]
      (let [char (first rem)]
        (if char
          (recur (reduce #(into %1 %2) []
                         (map #(inject-at-all-pos % char) perms))
                 (next rem))
          perms)))))

(defn- str-swap-chars-at [^String s, ^long i, ^long j]
  (let [chars (vec s)
        char1 (chars i)
        char2 (chars j)
        swapped (assoc chars
                  i char2
                  j char1)]
    (apply str swapped)))

(defn- str-swap-chars-at! [^String s, ^long i, ^long j]
  (let [chars (.toCharArray s)
        char1 (aget chars i)
        char2 (aget chars j)]
    (aset chars i char2)
    (aset chars j char1)
    (apply str chars)))

(defn- swaps [^String s, ^long i]
  (map #(str-swap-chars-at! s i %)
       (range (inc i) (.length s))))

(defn manoj [^String s]
  (reduce (fn [perms, i]
            (reduce #(into %1 %2) perms
                    (map #(swaps % i) perms))
            ) [s] (range 0 (.length s))))
