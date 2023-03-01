(ns solver2
  (:require [utilities :as utils]
            [solver :as solv]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            [clj-async-profiler.core :as flame]))

(defn- get-rowsets [rows wordset]
  (map (fn [[i l]] (filter #(= (get % i) l) wordset)) rows))

(defn- get-word-linkset [word row2-words link-fns]
  (let [compatible? (fn [next-word] (every? true? (map #(%1 %2 %3) link-fns word next-word)))]
    (p :T (filter compatible? row2-words))))

(defn- row-linkset [row next-row link]
  (let [link-bools    (map #(contains? (set link) %) (range 5))
        link-fns      (map #(if % = not=) link-bools)]
          (filter #(not-empty (nth % 1))
             (map #(list % (get-word-linkset % next-row link-fns)) row))))

;; NOTE: modified DFS for structure of linksets & 'all permuations' goal
(defn- dfs [solutions linksets wordlinks path]
   (let [word       (first wordlinks)
         adj-words  (second wordlinks)
         path       (conj path word)]
       (doseq [w adj-words]
        (if (empty? linksets)
          (swap! solutions conj (conj path w))
          (some #(when (= w (first %))
                  (dfs solutions (rest linksets) % path))
                (first linksets))))))

#_(defn- dfs-map [solutions linksets wordlinks path]
   (let [word       (key wordlinks)
         adj-words  (val wordlinks)
         path       (conj path word)]
       (doseq [w adj-words]
        (if (empty? linksets)
          (swap! solutions conj (conj path w))
          (some #(when (= w (first %))
                  (dfs solutions (rest linksets) % path))
                (first linksets))))))

(defn- get-solutions [linksets]
  (let [solutions (atom '())]
    (doseq [wordset (first linksets)]
      (dfs solutions (rest linksets) wordset []))
     @solutions))


(defn test-linksets [rowsets links]
  (doall (map row-linkset rowsets (rest rowsets) links)))

(defn linksets [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)
        all-linksets    (map row-linkset rowsets (rest rowsets) links)]
     (doall all-linksets)))

(defn solutions [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)
        all-linksets    (map row-linkset rowsets (rest rowsets) links)]
     (get-solutions all-linksets)))
