(ns solver4
  (:require [utilities :as utils]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            [clj-async-profiler.core :as flame]))

(add-basic-println-handler! {})

;; NOTE: Design a permutations accumulator that:
;;       Can be multithreaded
;;       Only does needed work!
;;          - This includes upstream work!
;;              - lazy eval vs eager eval
;;          - When +: use cache > recompute work
;;       Can be cached efficiently
;;       List v Vec for output
;;          - List makes more sense!
;;            tree leaves will be initial element
;;            earlier words will be added after & be earlier
;;
;; NOTE: Should be depth first:
;;          - Allows deep cache to build!
;;

;; NOTE: Do not pass complex datastructures as entries into memoized functions
;;       OR
;;       Use only simple args on Caching

(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])

(defn- get-all-row-subsets [rows wordset]
  (doall (map (fn [[i l]] (filter #(= (get % i) l) wordset)) rows)))


(defn- filter-row-by-word [word rowset [link-1 link-2]]
  (letfn [(compatible? [w]
            (every? true?
             (map #(if (or (= link-1 %) (= link-2 %))
                     (= (get word %) (get w %))
                     (not= (get word %) (get w %)))
                  (range 5))))]
    (doall (filter compatible? rowset))))


;; Is rows empty?
;;    yes - return word in a list
;;    no  -
;;      Generate Wordset
;;      Is wordset empty?
;;          yes - return nil
;;          no  -
;;            For each word -> recur
;;            Process return into '(("word" "word" "word") ("word" "word" "word") ("word" "word" "word"))
;;            cache it

(defn- dfs-eager [word i rows links]
  ())

(defn- get-solutions [[base-row & next-rows] links]
  (map #(dfs-eager % 0 next-rows links) base-row))

(defn solutions [puzzle wordset]
  (let [[rows links]    [(take-nth 2 puzzle) (take-nth 2 (rest puzzle))]
        rowsets         (get-all-row-subsets rows wordset)]
    (get-solutions rowsets links)))

(solutions input utils/all-words)

(profile
 {}
 (p :sol (solutions input utils/all-words)))

;; '(("word" "word" "word") ("word" "word" "word") ("word" "word" "word") ("word" "word" "word"))
;; ;; This
;; '({"word" ("word" "word") "word2" ("word" "word")}
;;   {"word" ("word" "word") "word2" ("word" "word")}
;;   {"word" ("word" "word") "word2" ("word" "word")})

;; (profile {}
;;          (dotimes [_ 100]
;;          (p :merge (merge '() '()))
;;          (p :big-merge (merge '((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)))
;;          (p :empty? (empty? '(((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10))))
;;          (p :map (map #(println %) '()))
;;          (p :count (count '(((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10))))
;;          (p :flat (flatten '("1" (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 2 3 4 (1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)))
;;          (p :apply (apply = true '(true true true true false false true true)))
;;          (p :every? (every? true? '(true true true true false false true true)))
;;          (p :reduce (reduce + 0 '(1 2 3 4 5 6 7 8 9 10)))))
