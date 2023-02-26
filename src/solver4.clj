(ns solver4
  (:require [utilities :as utils]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            [clj-async-profiler.core :as flame]))

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

(defn- dfs-old [solutions linksets wordlinks path]
   (let [word       (first wordlinks)
         adj-words  (second wordlinks)
         path       (conj path word)]
       (doseq [w adj-words]
        (if (empty? linksets)
          (swap! solutions conj (conj path w))
          (some #(when (= w (first %))
                  (dfs-old solutions (rest linksets) % path))
                (first linksets))))))

(defn- dfs-eager []
  ())


(defn solutions [puzzle wordset]
  (let [[rows links]    [(take-nth 2 puzzle) (take-nth 2 (rest puzzle))]
        rowsets         (get-all-row-subsets rows wordset)]
    rowsets))

(solutions input utils/all-words)

(add-basic-println-handler! {})
(profile {}
         (p :merge (merge '() '()))
         (p :big-merge (merge '((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)))
         (p :flat (flatten '("1" (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 2 3 4 (1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)))
         (p :apply (apply + '(1 2 3 4 5 6 7 8 9 10)))
         (p :reduce (reduce + 0 '(1 2 3 4 5 6 7 8 9 10))))

;; '(("word" "word" "word") ("word" "word" "word") ("word" "word" "word") ("word" "word" "word"))
;; ;; This
;; '({"word" ("word" "word") "word2" ("word" "word")}
;;   {"word" ("word" "word") "word2" ("word" "word")}
;;   {"word" ("word" "word") "word2" ("word" "word")})
