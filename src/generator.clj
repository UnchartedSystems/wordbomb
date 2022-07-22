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

;; NOTE Based on letter positions, find rows iteratively that satisfies constraints while equalizing frequency disparity
;; NOTE Letters ALMOST always match links 2 rows above.
;; This is because if the letter were in an independent position there would be 4 constraints on 5 spaces for a 2 link row, impossible
;; TODO REVIEW: Think about how to do this algorithmically


(defn get-unique [f previous]
  (assert (vector? previous) "'get-unique' takes a vector of values to match against")
  (first (filter (fn [i] (not-any? #(= i %) previous)) (repeatedly f))))

;; NOTE: alphabet char range, range end is exclusive
;; (map char (range 65 91))
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

;; TODO: Find better name!
;; TODO: Write valid subset for 3 triples as specified in remarkable notes
;; NOTE FIXME TODO: WAIT! Consider [0 1 2] [1 2 3] [1 2 4]! Valid subsets, but remarkable method doesn't apply!a
;; NOTE: If contains? returns same num for both pairs, then every subset that uses that num is valid
;; f([1 2 3] [0 1 2]) = 3, f([1 2 3] [1 2 4]) = 3, therefore [1 3], [2 3] both valid.

;; Assume 3 subsets, not 2
;; TODO: rename everything
(defn- comp-2-subsets [x y]
  (filter (fn [num] (not (some #(= num %) y))) x))

;; TODO: rename everything
(defn- valid-3-subsets [last this next]
  (list (comp-2-subsets this last) (comp-2-subsets this next)))

(valid-3-subsets [0 1 3] [0 2 3] [0 2 4])

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

