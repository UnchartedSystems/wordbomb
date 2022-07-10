(ns solver
  (:require [clojure.string :as string]))

;; Initial bruteforce, need more elegant solution when mass generating

;; 1 - Filter wordset for letter positions
;; 2 - FOR word in subset 1
;; 3 - Filter subset 2 to links 1 in subset 1
;; 4 - FOR word in filtered subset 2
;; 5 - filter subset 3 to link 2 in subset 2
;; etc
;; when count of filtered subset at some row is 0, break
;; when every word is filled, return a solution
;; same word can be involved in multiple solution
;; when every word is searched, return all solutions


(def word-set (set (map string/upper-case (string/split-lines (slurp "words.txt")))))
(def puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \A]])
;; "4P021N340F123C042A"

;; variable names are terrible, will come back to this
(defn- filter-subset-by-word [word sublinks word-subset]
  (letfn [(letters-valid? [new-word] (map (fn [i] ((if (some #(= % i) sublinks) = not=)
                                           (get word i) (get new-word i))) (range 5)))
          (word-valid? [word] (apply = (letters-valid? word)) )]
    (filter word-valid? word-subset)))

;; HOW AM I RETURNING ALL COMBINATIONS
(defn- iterate-over-subset [rem-sets rem-links]
  (map #(filter-subset-by-word % (first rem-links) (second rem-sets)) (first rem-sets)))

(defn solve [puzzle]
  (let [letters (vec (take-nth 2 puzzle))
        links   (vec (take-nth 2 (rest puzzle)))
        row-words (map (fn [[n l]] (filter #(= (get % n) l) word-set)) letters)]
    ;; (filter-subset-by-word "SHARP" (first links) (second row-words))
    (iterate-over-subset row-words links)))

;; Recursively map for word in subset (initialization + same func for every layer)
;; filter out nil
;;  if no next row in puzzle, return solution as seq of words
;;  else, filter next subset of words based on word
;;
(comment "wow how have I never seen this comment function before")

;;    if subset size = 0, return nil
;;    else map over next word subset, and update state used for counting
;; Return to?
;;
;;Note, Map is branching, is not the right construct for creating a flat list of solutions!



(solve puzzle)
