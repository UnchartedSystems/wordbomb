(ns solver4
  (:require [utilities :as utils]
            [taoensso.tufte :refer (defnp p profiled profile add-basic-println-handler!)]
            [clj-async-profiler.core :as flame]
            [clojure.core.cache.wrapped :as w]))

;; NOTE: Design a permutations accumulator that:
;;       Can be multithreaded
;;       Only does needed work!
;;          - This includes upstream work!
;;              - lazy eval vs eager eval
;;          - When +: use cache > recompute work
;;       Can be cached efficiently
;;       List v Vec for output
;;          - List makes more sense!
;;            tree leaves will be initial element
;;            earlier words will be added after & be earlier
;;
;; NOTE: Should be depth first:
;;          - Allows deep cache to build!
;;

;; NOTE: Do not pass complex datastructures as entries into memoized functions
;;       OR
;;       Use only simple args on Caching

;; NOTE: DFS design
;; Is rows empty?
;;    yes - return word in a list
;;    no  -
;;      Generate Wordset
;;      Is wordset empty?
;;          yes - return nil
;;          no  -
;;            For each word -> recur
;;            Process return into '(("word" "word" "word") ("word" "word" "word") ("word" "word" "word"))
;;            cache it




(defn- get-rowsets [rows wordset]
  (mapv (fn [[i l]] (filterv #(= (get % i) l) wordset)) rows))

(defn- get-linkset [word rowset [link-1 link-2]]
  (letfn [(compatible? [^java.lang.String w]
            (loop [i 0]
              (when (if (or (= link-1 i) (= link-2 i))
                      (.equals (nth word i) (nth w i))
                      (not (.equals (nth word i) (nth w i))))
                (if (= i 4) w (recur (inc i))))))]
    [word (filterv compatible? rowset)]))

(defn get-linksets [row next-row link]
  (persistent!
   (transduce (comp (map #(get-linkset % next-row link))
                    (filter #(not-empty (peek %))))
              conj! (transient {}) row)))

(defn- dfs [solutions [linkset & linksets] word path]
  (if (empty? linkset)
    (swap! solutions conj (conj path word))
    (when-let [adj-words  (get linkset word)]
      (doseq [w adj-words]
        (dfs solutions linksets w (conj path word))))))

(defn- get-solutions [linksets]
  (let [solutions (atom '())]
      (doall (pmap #(dfs solutions linksets % []) (keys (first linksets))))
     @solutions))

#_(def cache (w/fifo-cache-factory {}))
(defn- get-wordset [word rowset [link-1 link-2]]
  (letfn [(compatible? [^java.lang.String w]
            (loop [i 0]
              (when (if (or (= link-1 i) (= link-2 i))
                      (.equals (nth word i) (nth w i))
                      (not (.equals (nth word i) (nth w i))))
                (if (= i 4) w (recur (inc i))))))]
    (filterv compatible? rowset)))

(defn- dfs-b [word [link & links] [rowset & rowsets] solutions path]
  (if (empty? rowset)
    (swap! solutions conj (conj path word))
    (let [adj-words (get-wordset word rowset link)]
      (when (not-empty adj-words)
        (doseq [w adj-words]
          (dfs-b w links rowsets solutions (conj path word)))))))

(defn- get-solutions-b [links rowsets]
  (let [solutions (atom '())]
      (doall (pmap #(dfs-b % links (rest rowsets) solutions []) (first rowsets)))
     @solutions))

(defn linksets [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)
        all-linksets    (pmap get-linksets rowsets (rest rowsets) links)]
    #_(doall all-linksets)
    (doall (get-solutions all-linksets))))


(defn linksets-b [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)]
    (doall (get-solutions-b links rowsets))))
