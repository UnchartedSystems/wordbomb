(ns profile
  (:require [utilities :as utils]
            [solver :as s1]
            [solver2 :as s2]
            [solver4 :as s4]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            [clj-async-profiler.core :as flame]))


(set! *warn-on-reflection* true)
(add-basic-println-handler! {})

(def input [[4 \P] [2 0] [1 \N] [3 4] [0 \F] [1 2] [3 \C]])

(profile {} (dotimes [_ 3]
              (p :1-lazy  (s1/linksets input utils/all-words))
              #_(p :2   (s2/linksets  input utils/all-words))
              #_(p :4   (s4/linksets  input utils/all-words))))




#_(flame/profile (dotimes [_ 5] (solutions input utils/all-words)))
#_(flame/serve-ui 8080)


;; (profile {}
;;          (dotimes [_ 100]
;;          (p :merge (merge '() '()))
;;          (p :big-merge (merge '((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)))
;;          (p :empty? (empty? '(((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10))))
;;          (p :map (map #(println %) '()))
;;          (p :count (count '(((1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)) '((1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10))))
;;          (p :flat (flatten '("1" (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 2 3 4 (1 2 3 4 (1 2 3 4 (1 2 3 4 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10) 5 6 7 8 9 10) 5 6 7 (1 2 3 4 5 6 7 8 9 10) 8 9 10)))
;;          (p :apply (apply = true '(true true true true false false true true)))
;;          (p :every? (every? true? '(true true true true false false true true)))
;;          (p :reduce (reduce + 0 '(1 2 3 4 5 6 7 8 9 10)))))
