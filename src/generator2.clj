(ns generator2)


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
 [2 0]   ; 2, NOTE: (Difference: Lx Links || Lx+3 Letter Num) -> Lx+4 Link Num 2
 [1 \N]  ; 3,
 [3 4]   ; 4,
 [0 \F]  ; 5,
 [1 2]   ; 6,
 [3 \C]  ; 7,
 [0 4]   ; 8,
 [2 \O]  ; 9,
 [1 3]   ; 10,
 [4 \R]] ; 11,
