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

;; NOTE: DFS design
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


(defn- eager-get-rowsets [rows wordset]
  (doall (map (fn [[i l]] (filter #(= (get % i) l) wordset)) rows)))

(defn- get-linkset [word rowset [link-1 link-2]]
  (letfn [(compatible? [w]
            (every? true?
             (map #(if (or (= link-1 %) (= link-2 %))
                     (= (get word %) (get w %))
                     (not= (get word %) (get w %)))
                  (range 5))))]
    (doall (filter compatible? rowset))))


(defn- get-linksets [row next-row link]
    (into {}
          (filterv #(not-empty (peek %))
             (map #(vector % (get-linkset % next-row link)) row))))

(defn linksets [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (eager-get-rowsets rows wordset)
        all-linksets    (map get-linksets rowsets (rest rowsets) links)]
    (doall all-linksets)))
