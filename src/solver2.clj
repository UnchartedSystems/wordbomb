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


(defn- filter-row-by-word [word row-set row-links]
  (assert (string? word) "wrong type: 'word' is not string")
  (letfn [(linked? [i] (if (some #(= % i) row-links) = not=))
          (letters-valid? [new-word] (mapv #((linked? %) (get word %) (get new-word %)) (range 5)))
          (word-valid? [word] (apply = true (letters-valid? word)))]
    (filter word-valid? row-set)))

(defn- valid-row [this-row next-row row-links]
  (let [valid-wordset (fn [w] (filter-row-by-word w next-row row-links))
        filter-empties #(if (empty? %3) %1 (assoc %1 %2 %3))]
    (reduce #(filter-empties %1 %2 (valid-wordset %2)) {} this-row)))


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


;; TODO: Find a performant blend of lazyness + good code
;;       I think the main perf gains of s1-ls is lazy eval can finish early
;;       means my intuition could be right about s2-ls being more efficient
;;       need to research more and maybe harness lazy evaluation + maps for s2-ls
;;       maybe finally get some multithreading action in.
;;
(defn solutions [puzzle wordset]
  (let [[rows links]    [(take-nth 2 puzzle) (take-nth 2 (rest puzzle))]
        rowsets         (get-all-row-subsets rows wordset)
        linksets        (p :s2-ls (doall (get-rows-linksets rowsets links)))
        linksets        (p :s1-ls (doall (map valid-row rowsets (rest rowsets) links)))
        ]
    (p :solutions (get-solutions linksets))))

(profile {} (dotimes [_ 10] (p :s2 (solutions input utils/all-words))))
