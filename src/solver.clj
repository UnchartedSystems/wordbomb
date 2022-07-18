(ns solver
  (:require [utilities :as utils]))

;; NOTE: A fitness function that values curated words more then all words would be useful
;; HACK REVIEW TODO: Rewrite Solution Search to not blow the stack and allow concurrency
;; NOTE TODO: Add multitasking support!
;; TODO: For creating row-sets!
;; TODO: For creating valid-rows
;; TODO: For rewriting Solution Search


(defn- filter-row-by-word [word row-set row-links]
  (assert (string? word) "wrong type: 'word' is not string")
  (letfn [(linked? [i] (if (some #(= % i) row-links) = not=))
          (letters-valid? [new-word] (mapv #((linked? %) (get word %) (get new-word %)) (range 5)))
          (word-valid? [word] (apply = true (letters-valid? word)))]
    (filter word-valid? row-set)))

(defn- valid-row [this-row next-row row-links]
  (let [valid-wordset (fn [w] (filter-row-by-word w next-row row-links))
        filter-empties #(if (empty? %3) %1 (assoc %1 %2 %3))]
    (reduce #(filter-empties %1 %2 (valid-wordset %2)) {} this-row)))

(defn- valid-rows [puzzle word-set]
  (let [letters (vec (take-nth 2 puzzle))
        links   (vec (take-nth 2 (rest puzzle)))
        row-sets (mapv (fn [[n l]] (filter #(= (get % n) l) word-set)) letters)]
    (map valid-row row-sets (rest row-sets) links)))

;; HACK: builds stack frames via mapped recursion
(defn- solution-search
  ([word rows-left prev-words]
   (if (empty? rows-left)
     (conj prev-words word)
     (let [next-words (get (first rows-left) word)]
       (map #(solution-search % (rest rows-left) (conj prev-words word)) next-words))))
  ([rows]
   (map #(solution-search (key %) rows []) (first rows))))

;; HACK: downstream of stack recursion hack
(defn- cleanup [puzzle solutions]
  (let [len (count (vec (take-nth 2 puzzle)))]
    (partition len (flatten solutions))))

;; Used for triple checking core solution count from all solutions
(defn- filter-core [solutions]
  (let [core-word? (fn [w] (some #(= w %) utils/core-words))
        core-solution? (fn [s] (apply = true (map core-word? s)))]
    (filter core-solution? solutions)))

(defn core-solutions [puzzle]
  (cleanup puzzle (solution-search (valid-rows puzzle utils/core-words))))

#_(cleanup (solution-search (valid-rows test-puzzle all-words)))
#_(count (filter-core (cleanup (solution-search (valid-rows test-puzzle all-words)))))
(cleanup utils/test-puzzle (solution-search (valid-rows utils/test-puzzle utils/core-words)))
#_(valid-rows test-puzzle all-words)

(+ 1 1)
