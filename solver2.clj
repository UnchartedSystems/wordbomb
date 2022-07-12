(ns solver2
  (:require [clojure.string :as string]))

(def word-set (set (map string/upper-case (string/split-lines (slurp "words.txt")))))
(def puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])

(defn- filter-row-by-word [word row-set row-links]
  (assert (string? word) "wrong type: 'word' is not string")
  (letfn [(linked? [i] (if (some #(= % i) row-links) = not=))
          (letters-valid? [new-word] (mapv #((linked? %) (get word %) (get new-word %)) (range 5)))
          (word-valid? [word] (apply = (letters-valid? word)))]
    (filter word-valid? row-set)))

;; TODO: function is poorly named
;; TODO: Lots of names here are bad, come back and improve them. ('mapped-rows?', 'filter-entries')
(defn convert-row [this-row next-row row-links]
  (let [linked-words (fn [w] (filter-row-by-word w next-row row-links))
        filter-entries #(if (empty? %3) %1 (assoc %1 %2 %3))
        mapped-rows (reduce #(filter-entries %1 %2 (linked-words %2)) {} this-row)]
     mapped-rows))

(defn solve [puzzle]
  (let [letters (vec (take-nth 2 puzzle))
        links   (vec (take-nth 2 (rest puzzle)))
        row-sets (mapv (fn [[n l]] (filter #(= (get % n) l) word-set)) letters)]
    (convert-row (first row-sets) (second row-sets) (first links))))

(count (solve puzzle))

;; Create a map of every word to the words it connects to on the next row.
