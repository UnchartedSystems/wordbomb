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


(defn- get-rowsets [rows wordset]
  (mapv (fn [[i l]] (filterv #(= (get % i) l) wordset)) rows))

(defn- get-linkset [^java.lang.String word rowset [link-1 link-2]]
  (letfn [(compatible? [^java.lang.String w]
            (loop [i 0]
              (when (if (or (= link-1 i) (= link-2 i))
                      (.equals (nth word i) (nth w i))
                      (not (.equals (nth word i) (nth w i))))
                (if (= i 4) w (recur (inc i))))))]
    [word (filterv compatible? rowset)]))

(defn- get-linksets [row next-row link]
  (filterv #(not-empty (nth % 1))
           (mapv #(list % (get-linkset % next-row link)) row)))

(defn get2 [row next-row link]
  (persistent!
   (reduce #(if (not-empty (peek  %2))
              (conj! %1 %2) %1)
           (transient {})
           (mapv #(get-linkset % next-row link) row))))

(defn get3 [row next-row link]
  (into {} (comp (map #(get-linkset % next-row link))
                 (filter #(not-empty (peek %))))
        row))

(defn get4 [row next-row link]
  (persistent!
   (transduce (comp (map #(get-linkset % next-row link))
                    (filter #(not-empty (peek %))))
              conj!
              (transient {})
              row)))

(defn linksets [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)
        all-linksets    (pmap get-linksets rowsets (rest rowsets) links)]
    (doall all-linksets)))

(defn linksets2 [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)
        all-linksets    (pmap get2 rowsets (rest rowsets) links)]
    (doall all-linksets)))


(defn linksets3 [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)
        all-linksets    (pmap get3 rowsets (rest rowsets) links)]
    (doall all-linksets)))

(defn linksets4 [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)
        all-linksets    (pmap get4 rowsets (rest rowsets) links)]
    (doall all-linksets)))
