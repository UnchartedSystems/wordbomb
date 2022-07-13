(ns solver
  (:require [clojure.string :as string]))

;; NOTE: a function to filter end results to solutions that only use curated words would be very useful
;; NOTE: alternatively a fitness function that values curated words more

(def all-words (set (map string/upper-case (string/split-lines (slurp "words-all.txt")))))
(def core-words (set (map string/upper-case (string/split-lines (slurp "words-core.txt")))))

(def puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])

(defn- filter-row-by-word [word row-set row-links]
  (assert (string? word) "wrong type: 'word' is not string")
  (letfn [(linked? [i] (if (some #(= % i) row-links) = not=))
          (letters-valid? [new-word] (mapv #((linked? %) (get word %) (get new-word %)) (range 5)))
          (word-valid? [word] (apply = true (letters-valid? word)))]
    (filter word-valid? row-set)))

(filter-row-by-word "CLACK" ["PLACE"] [0 4])

;; TODO: function is poorly named, rename
;; TODO: Lots of names here are bad, come back and improve them. ('mapped-rows?', 'filter-entries')
(defn- convert-row [this-row next-row row-links]
  (let [linked-words (fn [w] (filter-row-by-word w next-row row-links))
        filter-entries #(if (empty? %3) %1 (assoc %1 %2 %3))
        mapped-rows (reduce #(filter-entries %1 %2 (linked-words %2)) {} this-row)]
    mapped-rows))

;; TODO: function is poorly named, rename
(defn- solve [puzzle word-set]
  (let [letters (vec (take-nth 2 puzzle))
        links   (vec (take-nth 2 (rest puzzle)))
        row-sets (mapv (fn [[n l]] (filter #(= (get % n) l) word-set)) letters)]
    (map convert-row row-sets (rest row-sets) links)))

;; HACK: builds stack frames via mapped recursion
(defn- solution-search
  ([word rows-left prev-words]
   (if (empty? rows-left)
     (conj prev-words word)
     (let [next-words (get (first rows-left) word)]
       (if (empty? next-words)
         false
         (map #(solution-search % (rest rows-left) (conj prev-words word)) next-words)))))
  ([rows]
   (map #(solution-search (key %) rows []) (first rows))))

;; HACK: downstream of stack recursion hack
(defn- cleanup [solutions]
  (let [len (count (vec (take-nth 2 puzzle)))]
    (partition len (filter boolean (flatten solutions)))))

;; Used for triple checking core words solution count
(defn- filter-core [solutions]
  (let [core-word? (fn [w] (some #(= w %) core-words))
        core-solution? (fn [s] (apply = true (map core-word? s)))]
    (filter core-solution? solutions)))

#_(count (cleanup (solution-search (solve puzzle all-words))))
#_(count (filter-core (cleanup (solution-search (solve puzzle all-words)))))
#_(count (cleanup (solution-search (solve puzzle core-words))))
