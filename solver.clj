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
  (letfn [(letters-valid? [new-word] (map (fn [i] ((if (some #(= % i) row-links) = not=)
                                           (get word i) (get new-word i))) (range 5)))
          (word-valid? [word] (apply = (letters-valid? word)) )]
    (filter word-valid? row-set)))

(defn- filter-row-by-row [last-row rows-left links-left]
  (let [this-row (first rows-left)
        row-links (first links-left)
        filter-linked-row #(filter-row-by-word % this-row row-links)]
    (filter #(not-empty %) (map filter-linked-row last-row))))

;; results do nothing, no passing back of chain of words, just recursion
(defn- hub [rows-left links-left results]
  (if (empty? (rest rows-left))
    results
    (recur (rest rows-left)
           (rest links-left)
           (filter-row-by-row (first rows-left) (rest rows-left) links-left))))

(defn solve [puzzle]
  (let [letters (vec (take-nth 2 puzzle))
        links   (vec (take-nth 2 (rest puzzle)))
        row-sets (map (fn [[n l]] (filter #(= (get % n) l) word-set)) letters)]
    (hub row-sets links [])))

(count (set (flatten (solve puzzle))))
