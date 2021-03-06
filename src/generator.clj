(ns generator
  (:require [clojure.string :as str]
            [solver :as solver]
            [utilities :as utils]))


;; NOTE TODO: # of Solutions isn't good enough! need to measure bottleneck of words across puzzle!
;; A puzzle could have many solutions, but only have 1 possible word in the middle!

;; NOTE REVIEW TODO: All generated puzzles should have these properties
;; No link can connect a set letter
;; each link set, and each letter, should be maximally distinct.
;; For letter, that means the first 5 are always distinct
;; For link sets, it means new links should tend towards spots with the lowest frequencies

(defn get-unique [f previous]
  (assert (vector? previous) "'get-unique' takes a vector of values to match against")
  (first (filter (fn [i] (not-any? #(= i %) previous)) (repeatedly f))))

;; NOTE: alphabet char range, range end is exclusive:
#_(map char (range 65 91))              ;every letter of the alphabet
(defn- new-letter []
  (char (+ 65 (rand-int 25))))

(defn- new-column []
  (rand-int 5))

#_(defn- links-validity? [all-links]
  (let [distinct-adjacencies? (fn [x y] (apply distinct? (flatten (list x y))))]
    (apply = true (map #(distinct-adjacencies? (nth all-links %)
                                               (nth all-links (inc %)))
                       (range (dec (count all-links)))))))

#_(links-validity? [[1 4] [0 3] [1 2] [0 4]])

;; NOTE FIXME TODO: WAIT! Consider [0 1 2] [1 2 3] [1 2 4]! Valid subsets, but remarkable method doesn't apply!a
;; NOTE: If contains? returns same num for both pairs, then every subset that uses that num is valid
;; f([1 2 3] [0 1 2]) = 3, f([1 2 3] [1 2 4]) = 3, therefore [1 3], [2 3] both valid.



(defn- filter-adjacency [links filter-against]
  (filter (fn [num] (not (some #(= num %) filter-against))) links))

;; TODO: rename everything
(defn- filter-adjacencies [prev this next]
  (list (filter-adjacency this prev) (filter-adjacency this next)))


;; TODO: rename input
(defn- link-subsets [input]
  (loop [outer-pos (take (dec (count input)) input)
         inner-pos (drop 1 input)
         out []]
    (if (empty? outer-pos) out
        (recur (rest outer-pos)
               (rest inner-pos)
               (loop [i-pos inner-pos
                      o out]
                 (if (empty? i-pos)
                   o
                   (recur (rest i-pos)
                          (conj o [(first outer-pos) (first i-pos)]))))))))


(defn- valid-columns [prev next]
  (filterv #(not (or (= % prev) (= % next))) (range 5)))

(defn- get-possible-links [letters]
  (mapv valid-columns letters (rest letters)))


(def test-letters (take 5 (distinct (repeatedly new-column))))

(def test-linksets (get-possible-links test-letters))

test-linksets
(mapv distinct
      (mapv #(mapv sort %)
            (mapv (fn [subsets] (filter #(not= (first %) (second %)) subsets))
                  (mapv #(vec (link-subsets (flatten %)))
                        (#(mapv filter-adjacencies % (rest %) (rest (rest %))) (stuff-linksets test-linksets))))))

(defn- all-link-supersets [linksets]
  (let [stuffed-linksets (apply vector [-1 -1 -1] (conj linksets [-1 -1 -1]))
        possible-links #(mapv filter-adjacencies % (rest %) (rest (rest %)))]
    "PLACEHOLDER - MAP PAIRS OUT OF LEFT TO RIGHT AND FILTER OUT DUPLICATES"
    ))

#(mapv filter-adjacencies % (rest %) (rest (rest %))) (stuff-linksets test-linksets)
