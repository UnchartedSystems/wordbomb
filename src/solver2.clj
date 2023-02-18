(ns solver2
  (:require [utilities :as utils]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; We'll request to send `profile` stats to `println`:
(tufte/add-basic-println-handler! {})

;;; Let's define a couple dummy fns to simulate doing some expensive work
(defn get-x [] (Thread/sleep 500)             "x val")
(defn get-y [] (Thread/sleep (rand-int 1000)) "y val")

;; How do these fns perform? Let's check:

(profile ; Profile any `p` forms called during body execution
  {} ; Profiling options; we'll use the defaults for now
  (dotimes [_ 5]
    (p :get-x (get-x))
    (p :get-y (get-y))))


;; TODO:
;;  Take in wordset & puzzle
;;  PARALLELIZE: Get row-compatible subset per row
;;  EITHER:
;;    PARALLELIZE:  per row-subset, get next-row-compatible subset per word
;;    OR
;;    PARALLELIZE:  per word, get the row-compatible subset for the next row.
;;                  Work backwards, so that if a word has no compatible words
;;                  in the next row, eliminate it from the subset for selection
;;                  for the same process in the previous row
;;  Get all possible row combinations from resulting data structure and clean it up
