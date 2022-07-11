(ns solver
  (:require [clojure.string :as string]))

;; Initial bruteforce, need more elegant solution when mass generating

;;;; Optimization consideration:
;; The default collection of filtered subset words belonging to every
;; filtering word is 4x larger then the distinct set of those filtered subset words. This means 3
;; redundant ops for every required op
(comment #(set (flatten %)))

(def word-set (set (map string/upper-case (string/split-lines (slurp "words.txt")))))
(def puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])
;; "4P021N340F123C042A"

(defn- filter-row-by-word [word row-set row-links]
  (assert (string? word) "wrong type: 'word' is not string")
  (letfn [(linked? [i] (if (some #(= % i) row-links) = not=))
          (letters-valid? [new-word] (mapv #((linked? %) (get word %) (get new-word %)) (range 5)))
          (word-valid? [word] (apply = (letters-valid? word)))]
    (filter word-valid? row-set)))

(defn- filter-row-by-row [last-row next-row row-links]
  (let [filter-next-row  #(filter-row-by-word % next-row row-links)
        nested-hack      #(if (not (seq? %)) (filter-next-row %) (filter-row-by-row % next-row row-links))
        filter-dead-ends #(filter not-empty %)]
    (assert (seq? last-row) "Wrong type: 'last row' is not seq")
    (filter-dead-ends (mapv nested-hack last-row))))

(defn- hub
  ([last-row rows-left links-left]
    (println last-row)
   (if (empty? rows-left)
     last-row
     (recur (filter-row-by-row last-row (first rows-left) (first links-left))
            (rest rows-left)
            (rest links-left))))
  ([row-sets links]
   (hub (filter-row-by-row (first row-sets) (second row-sets) (first links))
        (rest (rest row-sets))                 ; THIS WAS THE BUG AURGGHGHGHGHGHGHGHG
        (rest links))))

;;;; Major structural flaw
;; Each set passed in builds its own set of word subsets, so at each layer of recurions word sets are more deeply nested.
;; The actual state of the previous words isn't captured in these sets of sets of sets, but each successive layer of set needs to be mapped.
;; This is a stupid solution, needs a proper fix. number of mappings can scale with a counter, but don't do this.

(defn solve [puzzle]
  (let [letters (vec (take-nth 2 puzzle))
        links   (vec (take-nth 2 (rest puzzle)))
        row-sets (mapv (fn [[n l]] (filter #(= (get % n) l) word-set)) letters)]
    (doall (hub row-sets links))))

(println "---")
(def test-sets (mapv (fn [[n l]] (filter #(= (get % n) l) word-set)) (vec (take-nth 2 puzzle))))
(def test-links (vec (take-nth 2 (rest puzzle))))

#_(def test-results
  (filter-row-by-row
   (filter-row-by-row
    (filter-row-by-row
     (filter-row-by-row (get test-sets 0) (get test-sets 1) (get test-links 0))
     (get test-sets 2) (get test-links 1))
    (get test-sets 3) (get test-links 2))
   (get test-sets 4) (get test-links 3)))

(println (count (flatten (solve puzzle))))
