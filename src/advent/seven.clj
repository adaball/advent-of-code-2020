(ns advent.seven
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]))

(defn get-input []
  (let [raw-text (slurp "resources/seven.csv")
        lines    (s/split raw-text #"\n")
        lines    (map #(s/split % #",\s?") lines)]
    lines))

(defn line->rule [line]
  "input line to rule
  
   input line structure:
     light orange bags contain 1 dark maroon bag, 3 dim maroon bags, 5 striped green bags, 2 pale aqua bags.
   rule structure: 
     {\"light orange\" {\"dark maroon\" 1 \"dim maroon\" 3 \"striped green\" 5 \"pale aqua\" 2}}"
  (let [[root child-0] (map #(s/replace % #"\s?bags?" "") (-> line first (s/split #" contain ")))
        bag-fn         (fn [[n bag]] {bag (if (nil? n) nil (Integer/parseInt n))})
        children       (->> (conj (rest line) child-0)
                            (map #(s/replace % #"\s?bags?\.?" ""))
                            (map #(re-seq #"(\d+)\s{1}([a-z\s]+)" %)) 
                            (map flatten) 
                            (map rest)
                            (map bag-fn)
                            (reduce merge {}))]
    {root (if (= {nil nil} children) nil children)}))

(defn get-rules []
  (->> (get-input)
       (map line->rule)
       (reduce merge {})))

(def solution-one-counter (atom 0))

(defn find-shiny-gold [rule-entry rules]
  (let [inc-cnt (fn [] (swap! solution-one-counter inc))
        entry-v (second rule-entry)
        loop-fn (fn [rule-v]
                  (loop [loop-v rule-v]
                    ; if there are no child vals to work with, end loop
                    (if (empty? loop-v)
                      nil
                      ; search child vals looking for a match
                      (if (> (count (filter #(= "shiny gold" (key %)) loop-v)) 0)
                        (do (inc-cnt) nil)
                        ; replace any child vals having a non-nil amount by looking up the entry in the rules map
                        ; by color, replacing it with only the value found there.  this way we traverse the entire
                        ; rule set.
                        (recur (reduce (fn [acc [color amnt]]
                                         (if (nil? amnt)
                                           acc
                                           (merge acc (get rules color))))
                                       {}
                                       loop-v))))))]
    (loop-fn entry-v)))

(defn solve-one []
  (let [rules (get-rules)]
    (reset! solution-one-counter 0)
    (doseq [entry (get-rules)]
      (find-shiny-gold entry rules))
    @solution-one-counter))

