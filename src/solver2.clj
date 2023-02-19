(ns solver2
  (:require [utilities :as utils]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; We'll request to send `profile` stats to `println`:
(tufte/add-basic-println-handler! {})

;; TODO:
;;  Take in wordset & puzzle
;;  PARALLELIZE: Get row-compatible subset per row
;;    NOTE: nope, overhead too high
;;  EITHER:
;;    PARALLELIZE:  per row-subset, get next-row-compatible subset per word
;;    OR
;;    PARALLELIZE:  per word, get the row-compatible subset for the next row.
;;                  Work backwards, so that if a word has no compatible words
;;                  in the next row, eliminate it from the subset for selection
;;                  for the same process in the previous row
;;  Get all possible row combinations from resulting data structure and clean it up



(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])

; [4 \P]

(defn- get-row-subset [row-constraint wordset]
  (let [compatible? (fn [word] (= (get word (first row-constraint))
                                  (second row-constraint)))]
    (filter compatible? wordset)))

(def mock-rows [[4 \P] [1 \N] [0 \F] [3 \C]])

(defn- get-all-row-subsets [rows-constraints wordset]
  (mapv #(vec (get-row-subset % wordset)) rows-constraints))

(def mock-next-row [2 0])

;FIXME: rename
(defn filter-to-linked-words [link-fns word next-row]
  (let [compatible? (fn [next-word] (apply = true (map #(%1 %2 %3) link-fns word next-word)))]
    (filter compatible? next-row)))

; FIXME: rename
; TODO: save new subset for next base-row & save base-words -> next-words
(defn- x-setup [link-constraint base-row next-row]
  (let [link-bools (mapv #(contains? (set link-constraint) %) (range 5))
        link-fns (mapv #(if % = not=) link-bools)]
    (mapv #(filter-to-linked-words link-fns % next-row) base-row)))

(x-setup [2 0]
         (first (get-all-row-subsets mock-rows utils/core-words))
         (second (get-all-row-subsets mock-rows utils/core-words)))

#_(profile
 {}
 (dotimes [_ 5]
   (p :l (get-all-row-subsets mock-input utils/core-words))
   (p :v (get-all-row-subsetsv mock-input utils/core-words))))
