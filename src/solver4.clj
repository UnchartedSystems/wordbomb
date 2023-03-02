(ns solver4)

(defn- get-rowsets [rows wordset]
  (mapv (fn [[i l]] (filterv #(= (get % i) l) wordset)) rows))

(defn- get-wordset [word rowset [link-1 link-2] cache id]
  (letfn [(compatible? [^java.lang.String w]
            (loop [i 0]
              (when (if (or (= link-1 i) (= link-2 i))
                      (.equals (nth word i) (nth w i))
                      (not (.equals (nth word i) (nth w i))))
                (if (= i 4) w (recur (inc i))))))]
    (let [kw (keyword (str word id))]
      (if-let [answer (kw @cache)]
        answer
        (let [answer (filterv compatible? rowset)]
          (swap! cache assoc kw answer)
          answer)))))

(defn- dfs [word [link & links] [rowset & rowsets] cache solutions path counter]
  (if (not (pos? counter))
    (swap! solutions conj (conj path word))
    (let [adj-words (get-wordset word rowset link cache counter)]
      (when (not-empty adj-words)
        (doseq [w adj-words]
          (dfs w links rowsets cache solutions (conj path word) (dec counter)))))))

(defn- get-solutions [links rowsets length]
  (let [cache (atom {})
        solutions (atom '())]
      (doall (pmap #(dfs % links (rest rowsets) cache solutions [] (dec length)) (first rowsets)))
     @solutions))

(defn solutions [puzzle wordset]
  (let [rows            (take-nth 2 puzzle)
        length          (count rows)
        links           (take-nth 2 (rest puzzle))
        rowsets         (get-rowsets rows wordset)]
    (if (every? not-empty rowsets)
      (doall (get-solutions links rowsets length)) '())))
