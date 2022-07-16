(ns generator
  (:require [clojure.string :as str]
            [solver :as solver]))


(def test-puzzle [[4 \P] [0 2] [1 \N] [3 4] [0 \F] [1 2] [3 \C] [0 4] [2 \O]])
(def string-puzzle "4P021N340F123C042O")

(defn- split-pairs [seq]
  (loop [i seq, o []] (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2))))))

(defn- puzzle-str-to-vec [s]
  (let [parse #(if (int? (read-string %)) (read-string %) (get % 0))
        split-pairs #(loop [i %, o []]
                       (if (empty? i) o (recur (subvec i 2) (conj o (subvec i 0 2)))))]
    (split-pairs (mapv parse (str/split s #"")))))

(defn- puzzle-vec-to-str [v] (apply str (flatten v)))

;; ---

;; NOTE: alphabet char range, range end is exclusive
#_(map char (range 65 91))

;; HACK TODO: Just made repeating vector maker to see if helper functions work
;; need to be reworked to incorporate previous numbers to be banned

;; NOTE TODO: # of Solutions isn't good enough! need to measure bottleneck of words across puzzle!
;; A puzzle could have many solutions, but only have 1 possible word in the middle!

;; NOTE REVIEW: This function is very dense, poorly named, and difficult to understand as is. Can be simplified.

(def debug #(do (println %) %))
(defn- generator [len]
  (let [make-new   (fn [v f] (first (filter (fn [i] (not-any? #(= i %) v)) (repeatedly f))))
        new-letter (fn [v] (make-new v #(char (+ 65 (rand-int 25)))))
        new-number (fn [v] (make-new v #(rand-int 5))) ;Not efficient, but also not hotloop

        letter-nums (take len (cycle (take 5 (distinct (repeatedly #(new-number []))))))

        new-links (fn [prev-links] (vec (take 2 (distinct (repeatedly #(new-number prev-links))))))
        all-links #((fn [c links] (if (< c 1 ) links (recur (dec c) (conj links (new-links (if (empty? links) [] (last links))))))) len [])
        row-nums (take len (cycle (take 5 (distinct (repeatedly #(new-number []))))))

        dist-nums ((fn [] (take len (cycle (take 5 (distinct (repeatedly #(new-number []))))))))]

    (map  (fn [i] (vec (take 2 (distinct (repeatedly #(new-number [(nth letter-nums i) (nth letter-nums (inc i))])))))) (range (dec len)))

    ;; (println letter-nums)
    ;; (println (nth letter-nums 1))
    ;; (println (nth letter-nums 2))

    ;; (take 10 (repeatedly #(new-number [1 2])))

    #_(all-links)

    #_(loop [c len, puzzle [], letters [], letter-nums [], last-nums []]
        (if (< c 1)
          puzzle
          (recur ())))
    ))

(generator 5)


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
;; NOTE Letters always match links 2 rows above.
;; This is because if the letter were in an independent position there would be 4 constraints on 5 spaces for a 2 link row, impossible
;; TODO REVIEW: Think about how to do this algorithmically
