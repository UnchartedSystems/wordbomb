(ns solver2
  (:require [utilities :as utils]
            [solver :as solv]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; We'll request to send `profile` stats to `println`:

(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])

(defn- split [puzzle]
  (let [rows (take-nth 2 puzzle)
        links (take-nth 2 (rest puzzle))]
    [rows links]))

(defn- get-row-subset [row-constraint wordset]
  (let [compatible? (fn [word] (= (get word (first row-constraint))
                                  (second row-constraint)))]
    (filter compatible? wordset)))

(defn- get-all-row-subsets [rows-constraints wordset]
  (mapv #(vec (get-row-subset % wordset)) rows-constraints))


(defn- get-word-linkset [word link-fns row2-words]
  (let [compatible? (fn [next-word] (apply = true (map #(%1 %2 %3) link-fns word next-word)))]
    (filter compatible? row2-words)))

; TODO: save new subset for next base-row & save base-words -> next-words
(defn- get-row-linkset [link-constraint row-words row2-words]
  (let [link-bools    (mapv #(contains? (set link-constraint) %) (range 5))
        link-fns      (mapv #(if % = not=) link-bools)]
    (filterv #(not-empty (second %))
             (mapv #(list % (get-word-linkset % link-fns row2-words)) row-words))))

;; TODO: Iterate over rows, return list of linked-subsets
;; FIXME: rename
(defn- get-rows-linksets
  ([rows links] (get-rows-linksets (first rows) (rest rows) links []))
  ([row rows links linksets]
   (if (empty? rows)
     linksets
     (let [linkset  (get-row-linkset (first links) row (first rows))
           next-row (set (flatten (mapv #(second %) linkset)))]
       (recur next-row (rest rows) (rest links) (conj linksets linkset))))))

;; NOTE: modified DFS for structure of linksets & 'all permuations' goal
(defn- dfs [solutions linksets wordlinks path]
   (let [word       (first wordlinks)
         adj-words  (second wordlinks)
         path       (conj path word)]
     (doall
      (p :s2-for (for [w adj-words]
        (if (empty? linksets)
          (swap! solutions conj (conj path w))
          (some #(when (= w (first %))
                  (dfs solutions (rest linksets) % path))
                (first linksets))))))))

(defn- get-solutions [linksets]
  (let [solutions (atom [])]
    (doall (pmap #(dfs solutions (rest linksets) % []) (first linksets)))
     @solutions))

(defn solutions [puzzle wordset]
  (let [[rows links]    (p :s2-split (split puzzle))
        rowsets         (p :s2-rowsets (get-all-row-subsets rows wordset))
        linksets        (p :s2-linksets(get-rows-linksets rowsets links))]
    (p :s2-solutions (get-solutions linksets))))


(tufte/add-basic-println-handler! {})

(profile {}
         (dotimes [_ 5]
         #_(p :s1-core (solv/solutions input utils/core-words))
         #_(p :s2-core (solutions input utils/core-words))
         (p :s1-all (solv/solutions input utils/all-words))
         (p :s2-all (solutions input utils/all-words))))
