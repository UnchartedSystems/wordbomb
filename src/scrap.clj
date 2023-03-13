(ns scrap)

;; Just a temp place for old versions of code
; jesus this is ugly

; TODO: far rows matching!
(defn adj-row-legal? [rows links word words og-i adj-i]
  (let [adj-word      (get words adj-i)
        [row-l-pos row-l] (get rows adj-i)
        [link1 link2] (get links (min og-i adj-i))]
    (loop [l 0 letters '(1 2 3 4)]
      (if-not l
        [true (t/error-if-seen "adj-row-legal?")]
        (let [linked? (or (= link1 l) (= link2 l))
              word-l  (get word l)
              adj-l   (get adj-word l)]
          (if-not adj-word
            (cond
              (and linked? (not= word-l row-l) (= row-l-pos l))    [false (t/unmatched-linked l word word-l row-l og-i adj-i)]
              (and (not linked?) (= word-l row-l) (= row-l-pos l)) [false (t/matched-unlinked l word word-l row-l og-i adj-i)]
              :else                                                (recur (first letters) (rest letters)))
            (cond
              (and linked? (not= word-l adj-l))     [false (t/unmatched-linked l word adj-word word-l adj-l og-i adj-i)]
              (and (not linked?) (= word-l adj-l))  [false (t/matched-unlinked l word adj-word word-l adj-l og-i adj-i)]
              :else                                 (recur (first letters) (rest letters)))))))))

(defn- word-legal? [rows links words r-pos]
  (let [word            (get words r-pos)
        [l-pos letter]  (get rows r-pos)
        w-letter        (get word l-pos)
        adj+1           (adj-row-legal? rows links word words r-pos (inc r-pos))
        adj-1           (adj-row-legal? rows links word words r-pos (dec r-pos))]
    (cond (not= w-letter letter) [false (t/letter-mismatch l-pos word w-letter letter)]
          (not (first adj+1)) adj+1
          (not (first adj-1)) adj-1
          :else [true (t/error-if-seen "word-legal?")])))


;; :else (let [[word? feedback] (is-word? i)]
;;                      (if-not word?
;;                        (pl-do feedback (recur rows links words pos false))
;;                        (let [new-words        (assoc words pos i)
;;                              [legal? feedback] (word-legal? rows links new-words pos)]
;;                          (if-not legal?
;;                            (pl-do feedback (recur rows links words pos false))
;;                              (if-let [next-pos (get-next-pos pos rows new-words)]
;;                                (recur rows links new-words next-pos true)
;;                                (do (represent rows links new-words pos)
;;                                    (pl "YOU WIN")
;;                                    (identity :quit))))))))))))
