(ns solver2
  (:require [utilities :as utils]
            [solver :as solv]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            [clj-async-profiler.core :as flame]))

(set! *warn-on-reflection* true)
(add-basic-println-handler! {})

(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])

(defn- get-all-row-subsets [rows wordset]
  (map (fn [[i l]] (filter #(= (get % i) l) wordset)) rows))

(def ? #(do (println %) %))
(def n? #(do (println (count %)) %))


;; NOTE: S2
(defn- get-word-linkset [word row2-words link-fns]
  (let [compatible? (fn [next-word] (apply = true (map #(%1 %2 %3) link-fns word next-word)))]
    (filter compatible? row2-words)))


; TODO: save new subset for next base-row & save base-words -> next-words
(defn- row-linkset [row next-row link]
  (let [link-bools    (map #(contains? (set link) %) (range 5))
        link-fns      (map #(if % = not=) link-bools)]
    (into {}
          (filterv #(not-empty (peek %))
             (map #(vector % (get-word-linkset % next-row link-fns)) row)))))

(defn- row-linkset-b [row next-row link]
  (let [link-bools    (map #(contains? (set link) %) (range 5))
        link-fns      (map #(if % = not=) link-bools)]
          (into {}
          (filterv #(not-empty (peek %))
             (map #(vector % (get-word-linkset % next-row link-fns)) row)))))

(defn- row-linkset-c [row next-row [link1 link2]]
  (let [link-bools    (map #(if (or (== link1 %) (== link2 %)) true false) (range 5))
        link-fns      (map #(if % = not=) link-bools)]
          (filter #(not-empty (peek %))
             (map #(list % (get-word-linkset % next-row link-fns)) row))))

; FIXME: Confusing terrible perf
(defn- rows-linksets [row rows links linksets]
  (if (empty? rows)
    linksets
    (let [linkset  (doall (row-linkset-b row (first rows) (first links)))
          next-row (doall (set (flatten (map #(nth % 1) linkset))))]
      (recur next-row (rest rows) (rest links) (conj linksets linkset)))))


;; NOTE: S1
(defn- filter-row-by-word [word row-set row-links]
  (assert (string? word) "wrong type: 'word' is not string")
  (letfn [(linked? [i] (if (some #(= % i) row-links) = not=))
          (letters-valid? [new-word] (mapv #((linked? %) (get word %) (get new-word %)) (range 5)))
          (word-valid? [word] (apply = true (letters-valid? word)))]
    (filter word-valid? row-set)))

(defn- valid-row [this-row next-row row-links]
  (let [valid-wordset (fn [w] (filter-row-by-word w next-row row-links))]
    (filter #(not-empty (second %)) (map #(list % (valid-wordset %)) this-row))))

(defn- valid-row-old [this-row next-row row-links]
  (let [valid-wordset (fn [w] (filter-row-by-word w next-row row-links))
        filter-empties #(if (empty? %3) %1 (assoc %1 %2 %3))]
    (reduce #(filter-empties %1 %2 (valid-wordset %2)) {} this-row)))


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

(defn- dfs-map [solutions linksets wordlinks path]
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

(defn- get-solutions-b [linksets]
  (let [solutions (atom [])]
    (doseq [wordset (first linksets)]
      (dfs-map solutions (rest linksets) wordset []))
     @solutions))

(defn solutions [puzzle wordset]
  (let [[rows links]    [(take-nth 2 puzzle) (take-nth 2 (rest puzzle))]
        rowsets         (get-all-row-subsets rows wordset)
        linksets        (map row-linkset rowsets (rest rowsets) links)
        ]
     (get-solutions linksets)))

(defn solutions-b [puzzle wordset]
  (let [[rows links]    [(take-nth 2 puzzle) (take-nth 2 (rest puzzle))]
        rowsets         (get-all-row-subsets rows wordset)
        linksets        (rows-linksets (first rowsets) (rest rowsets) links [])]
     (get-solutions-b linksets)))

#_(solutions input utils/all-words)
;(count (set (flatten (solutions input utils/all-words))))

#_(flame/profile (dotimes [_ 5] (solutions input utils/all-words)))
#_(flame/serve-ui 8080)


(profile {} (dotimes [_ 7]
              (p :1b  (solv/solutions2 input utils/all-words))
              (p :2   (solutions input utils/all-words))
              (p :2b  (solutions-b input utils/all-words))
              ;(p :2c  (solutions-c input utils/all-words))
              ))
