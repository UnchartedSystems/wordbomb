(ns solver2
  (:require [utilities :as utils]
            [solver :as solv]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            [clj-async-profiler.core :as prof]))

(add-basic-println-handler! {})

;; NOTE: testing set perf on strings
(defn make-word
  ([] (make-word 26))
  ([alph]
   (let [get-char #(char (+ 65 (rand alph)))]
     (apply str (take 5 (repeatedly get-char))))))

(make-word)

#_(profile
 {}
 (dotimes [_ 5]
   (let [words   (p :words (take 10000 (repeatedly #(make-word 5))))
         wordset  (p :set   (set words))])))


(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])

(defn- get-all-row-subsets [rows wordset]
  (map (fn [[i l]] (filter #(= (get % i) l) wordset)) rows))

(def ? #(do (println %) %))
(def n? #(do (println (count %)) %))


(defn- get-word-linkset [word link-fns row2-words]
  (let [compatible? (fn [next-word] (apply = true (map #(%1 %2 %3) link-fns word next-word)))]
    (filter compatible? row2-words)))

; TODO: save new subset for next base-row & save base-words -> next-words
(defn- get-row-linkset [link-constraint row-words row2-words]
  (let [link-bools    (map #(contains? (set link-constraint) %) (range 5))
        link-fns      (map #(if % = not=) link-bools)]
    (filter #(not-empty (second %))
             (map #(list % (get-word-linkset % link-fns row2-words)) row-words))))

; FIXME: Confusing terrible perf
(defn- get-rows-linksets
  ([rows links] (get-rows-linksets (first rows) (rest rows) links []))
  ([row rows links linksets]
   (if (empty? rows)
     linksets
     (let [linkset  (p :ls (doall (get-row-linkset (first links) row (first rows))))
           next-row (p :set (doall (set (flatten (map #(nth % 1) linkset)))))]
       (recur next-row (rest rows) (rest links) (conj linksets linkset))))))



(defn- test-rows-linksets
  ([rows links] (test-rows-linksets (first rows) (rest rows) links []))
  ([row rows links linksets]
   (? rows)
   (if (empty? rows)
     linksets
     (let [linkset  (p :ls (doall (get-row-linkset (first links) row (first rows))))
           mp       (p :mp (doall (map #(second %) linkset)))
           flat     (p :flat (doall (flatten mp)))
           next-row (p :set (doall (set flat)))]
       #_(recur next-row (rest rows) (rest links) (conj linksets linkset))
       next-row))))


;; NOTE: modified DFS for structure of linksets & 'all permuations' goal
(defn- dfs [solutions linksets wordlinks path]
   (let [word       (first wordlinks)
         adj-words  (second wordlinks)
         path       (conj path word)]
      (for [w adj-words]
        (if (empty? linksets)
          (swap! solutions conj (conj path w))
          (some #(when (= w (first %))
                  (dfs solutions (rest linksets) % path))
                (first linksets))))))

(defn- get-solutions [linksets]
  (let [solutions (atom [])]
    (map #(dfs solutions (rest linksets) % []) (first linksets))
     @solutions))


(defn solutions [puzzle wordset]
  (let [[rows links]    [(take-nth 2 puzzle) (take-nth 2 (rest puzzle))]
        rowsets         (get-all-row-subsets rows wordset)
        linksets        (p :linksets (test-rows-linksets rowsets links))
        ]
    #_(p :solutions (get-solutions linksets))
    linksets))

get-rows-linksets
(profile {} (dotimes [_ 10] (p :s2 (solutions input utils/all-words))))
