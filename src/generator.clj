(ns generator
  (:require [clojure.string :as str]
            [solver :as solver]
            [utilities :as utils]))



;; ---

;; NOTE: alphabet char range, range end is exclusive
#_(map char (range 65 91))

;; HACK TODO: Just made repeating vector maker to see if helper functions work
;; need to be reworked to incorporate previous numbers to be banned

;; NOTE TODO: # of Solutions isn't good enough! need to measure bottleneck of words across puzzle!
;; A puzzle could have many solutions, but only have 1 possible word in the middle!

;; NOTE REVIEW: This function is very dense, poorly named, and difficult to understand as is. Can be simplified.




;; NOTE REVIEW TODO: All generated puzzles should have these properties
;; No link can connect a set letter
;; each link set, and each letter, should be maximally distinct.
;; For letter, that means the first 5 are always distinct
;; For link sets, it means new links should tend towards spots with the lowest frequencies

;; _ _ L _ _
;; o x o o x
;; _ _ _ L _
;; x o x o o
;; _ _ _ _ L
;; o x o x o
;; L _ _ _ _
;; o o x o x
;; _ L _ _ _

;; NOTE: Get all subsets of links
;; It's just sum of 4 + 3 + 2 + 1
(def link-subsets
  (loop [outer-pos (take 4 (range 5))
         inner-pos (drop 1 (range 5))
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

link-subsets

;; NOTE Based on letter positions, find rows iteratively that satisfies constraints while equalizing frequency disparity
;; NOTE Letters ALMOST always match links 2 rows above.
;; This is because if the letter were in an independent position there would be 4 constraints on 5 spaces for a 2 link row, impossible
;; TODO REVIEW: Think about how to do this algorithmically

(defn- generator [len]
  (let [make-new   (fn [v f] (first (filter (fn [i] (not-any? #(= i %) v)) (repeatedly f))))
        new-letter (fn [v] (make-new v #(char (+ 65 (rand-int 25)))))
        new-number (fn [v] (make-new v #(rand-int 5))) ]

    (take 5 (distinct (repeatedly #(new-number []))))

    ))

(apply distinct? (flatten [[1 3] [2 3]]))
(apply distinct? (flatten [[1 3] [2 4]]))

(defn- links-validity? [all-links]
  (let [distinct-adjacencies? (fn [x y] (apply distinct? (flatten (list x y))))]
    (apply = true (map #(distinct-adjacencies? (nth all-links %)
                                               (nth all-links (inc %)))
                       (range (dec (count all-links)))))))

(links-validity? [[1 4] [0 3] [1 2] [0 4]])

;; TODO: Find better name!
;; TODO: Write valid subset for 3 triples as specified in remarkable notes
;; NOTE FIXME TODO: WAIT! Consider [0 1 2] [1 2 3] [1 2 4]! Valid subsets, but remarkable method doesn't apply!a
;; NOTE: If contains? returns same num for both pairs, then every subset that uses that num is valid
;; f([1 2 3] [0 1 2]) = 3, f([1 2 3] [1 2 4]) = 3, therefore [1 3], [2 3] both valid.
(defn- valid-3-subsets [x y z]
  ())
