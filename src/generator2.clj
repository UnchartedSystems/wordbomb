(ns generator2)

;; :BUG: This approach (letters -> links), has been abandoned!
;;       letter & link positioning is codependant

;; NOTE REVIEW TODO: All generated puzzles should have these properties
;; No link can connect a set letter
;; each link set, and each letter, should be maximally distinct.
;; For letter, that means the first 5 are always distinct
;; For link sets, it means new links should tend towards spots with the lowest frequencies


;; NOTE: Generating Letters
;;        Easy solution:
;;        - Generate a range, and shuffle it
;;        - per element, make [element letter] where:
;;          - Letter is chosen randomly?
;;          - Letter is chosen based on compressed wordset frequency?
;;        - When range runs out, take last element,
;;          and generate a new shuffled range without that element, repeat
;;        - optional?: Check rowsets for letter viability
;;

;; NOTE: Generating Links
;;        Links are constrained because:
;;          - (2 links + 1 letter above) + (2 links + 1 letter below) = 6 potential constraints on 5 spaces!
;;          - This also means 3/5 spaces on a row are constrained!! links are determined by letters and first links


;; NOTE: Consider:
[[4 \P]  ; 1, NOTE: Lx Letter Num -> Lx+3 Link Num 1
 [2 0]   ; 2, NOTE: (Difference: Lx+4 Links || Lx+1 Letter Num) -> Lx+4 Link Num 2
 [1 \N]  ; 3, BUG: THIS DOES NOT HOLD!
 [3 4]   ; 4,
 [0 \F]  ; 5,
 [1 2]   ; 6,
 [3 \C]  ; 7,
 [0 4]   ; 8,
 [2 \O]  ; 9,
 [1 3]   ; 10,
 [4 \R]] ; 11,
;; NOTE: IF Lx-3/4 constraint doesn't exist: THEN *choose links*


;; TODO: stupid implementation for now, needs frequencies
(defn- get-letter [len]
  (let [alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    (take len (cycle (shuffle (map #(get alphabet %) (range 26)))))))

;; TODO: stupid implementation for now, needs noncyclical randomness
(defn- get-positions [len]
  (take len (cycle (shuffle (range 5)))))

(defn- make-rows [len]
   (let [positions (get-positions len)
         letters   (get-letter len)]
     (mapv vector positions letters)))

(defn- solve-constraints [constraints]
  (doall (println constraints))
  (let [constraints (set constraints)]
  (vec (take 2 (filter #(apply distinct? % constraints) (range 5))))))

(def ? #(do (println %1 %2) %2))

;; TODO: inefficient & messy, start with pos? -> len > n
(defn- make-link [coll element]
  (let [len (count coll)]
    (if (>= len 4)
      (conj coll [(nth (nth coll (- len 3)) 0)
                  (some #(and (not= % (nth (peek coll) 0)) %) (nth coll (- len 4)))]
            element)
      (if (>= len 2)
        (conj coll (solve-constraints
                    (apply list (nth (peek coll) 0)
                           (nth element 0)
                           (nth coll (- len 2)))) element)
        (conj coll (solve-constraints
                    (list (nth (peek coll) 0)
                          (nth element 0))) element)))))

;; (defn- create-link [coll element]
;;   (conj coll
;;         (solve-constraints
;;          (keep identity
;;                (apply list (nth (peek coll) 0) (nth element 0) (peek (pop coll)))))
;;   element))


(defn- make-links [rows]
  (reduce make-link [(nth rows 0)] (subvec rows 1)))

(defn generate [len]
  (make-links (make-rows len)))
