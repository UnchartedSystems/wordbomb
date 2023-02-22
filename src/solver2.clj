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


;; NOTE: S2
(defn- get-word-linkset [word row2-words link-fns]
  (let [compatible? (fn [next-word] (apply = true (map #(%1 %2 %3) link-fns word next-word)))]
    (filter compatible? row2-words)))

; TODO: save new subset for next base-row & save base-words -> next-words
(defn- row-linkset [row next-row link]
  (let [link-bools    (map #(contains? (set link) %) (range 5))
        link-fns      (map #(if % = not=) link-bools)]
    (filter #(not-empty (second %))
             (map #(list % (get-word-linkset % next-row link-fns)) row))))

; FIXME: Confusing terrible perf
(defn- get-rows-linksets
  ([rows links] (get-rows-linksets (first rows) (rest rows) links []))
  ([row rows links linksets]
   (if (empty? rows)
     linksets
     (let [linkset  (p :ls (doall (get-row-linkset (first links) row (first rows))))
           next-row (p :set (doall (set (flatten (map #(nth % 1) linkset)))))]
       (recur next-row (rest rows) (rest links) (conj linksets linkset))))))


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

;; NOTE: Maybe the reason s1 worked so well as lazy eval is because
;;       it was acting as the set does in s2! In a map data structure
;;       it might be that when the key is called in solutions the links
;;       are actually produced by rowsets!

;; NOTE: One way to see if this works is to attach a side effect to the
;;       code that uses the lazily evald map that accumulates a count, and
;;       seeing if that count is different then the count of the map itself

;; NOTE: New idea! by coupling the generation of linksets with the dfs
;;       we can generate only the words we need to.
;;       NOTE: this is made difficult by the need to avoid regenerating
;;       linkset '(word links). Possible solutions:
;;          - pass the linksets to the next word, requires new structure
;;            is not parallel!
;;          - create an atom for linksets: understand concurrency of atoms
;;          - use lazyness!

(defn solutions [puzzle wordset]
  (let [[rows links]    [(take-nth 2 puzzle) (take-nth 2 (rest puzzle))]
        rowsets         (get-all-row-subsets rows wordset)
        linksets1        (p :s2-ls (map row-linkset rowsets (rest rowsets) links))
        linksets2        (p :s3-ls (map valid-row rowsets (rest rowsets) links))
        ]
    [(p :s2-sol (get-solutions linksets1))
     (p :s3-sol (get-solutions linksets2))]))

(profile {} (dotimes [_ 3] (p :s2 (solutions input utils/all-words))))
