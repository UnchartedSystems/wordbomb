(ns solver2
  (:require [utilities :as utils]
            [taoensso.tufte :as tufte :refer (defnp- defnp p profiled profile)]))

;; TODO:
;;  - Fitness Function that values Curated Words more
;;  - Multithreaded parallelism for functions
;;  - Rewrite solution
;;  - FIXME: Rewrite Solution Search to not blow the stack and allow concurrency

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

(defn valid-rows [puzzle word-set]
  (let [letters (vec (take-nth 2 puzzle))
        links   (vec (take-nth 2 (rest puzzle)))
        row-sets (mapv (fn [[n l]] (filter #(= (get % n) l) word-set)) letters)]
    (map valid-row row-sets (rest row-sets) links)
  #_row-sets))

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

(defn- filter-core [solutions]
  (let [core-word? (fn [w] (some #(= w %) utils/core-words))
        core-solution? (fn [s] (apply = true (map core-word? s)))]
    (filter core-solution? solutions)))

(defn solutions [puzzle wordset]
  (cleanup puzzle (solution-search (valid-rows puzzle wordset))))

(defn linksets [puzzle wordset]
  (let [all-linksets (valid-rows puzzle wordset)]
  (doall all-linksets)))

(defn solutions2 [puzzle wordset]
  (let [x (valid-rows puzzle wordset)
        y (solution-search x)]
  (cleanup puzzle y)))

#_(cleanup (solution-search (valid-rows test-puzzle all-words)))
#_(count (filter-core (cleanup (solution-search (valid-rows test-puzzle all-words)))))
#_(cleanup utils/test-puzzle (solution-search (valid-rows utils/test-puzzle utils/core-words)))
#_(valid-rows test-puzzle all-words)
#_(valid-rows [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]] utils/all-words)
#_(core-solutions [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])
#_(count (solutions [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]] utils/all-words))
