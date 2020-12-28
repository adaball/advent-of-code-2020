(ns advent.seven
  (:require [clojure.string :as s]))

(defn get-input []
  (let [raw-text (slurp "resources/seven.csv")
        lines    (s/split raw-text #"\n")
        lines    (map #(s/split % #",\s?") lines)]
    lines))

; Rule Structure
; {"light orange" {"dark maroon" 1 "dim maroon" 3 "striped green" 5 "pale aqua" 2}}
(defn line->rule [line]
  (let [[root child-0] (map #(s/replace % #"\s?bags?" "") (-> line first (s/split #" contain ")))
        bag-fn         (fn [[n bag]] {bag (if (nil? n) nil (Integer/parseInt n))})
        children       (->> (conj (rest line) child-0)
                            (map #(s/replace % #"\s?bags\.?" ""))
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

(defn rule->node [rule]
  (let [parent      (first (keys rule))
        children    (first (vals rule))
        children-fn (fn [m]
                      (if (nil? m)
                        nil
                        (map (fn [bag-type n] (repeat n bag-type)) m)))
        children    (map children-fn children)]
    (list parent children)))

(rule->node {"light orange" nil})
(rule->node {"light orange" {"dark maroon" 1 "dim maroon" 3 "striped green" 5 "pale aqua" 2}})


; tree format
; (("light orange" ("dark maroon" 
;                   "dim maroon" "dim maroon" "dim maroon" 
;                   "striped green" "striped green" "striped green" "striped green" "striped green"
;                   "pale aqua" "pale aqua")))

(defn solve-one []
  (let [rules (get-rules)]
    rules))



(take 3 (map line->rule (get-input)))
(take 3 (map line->rule (get-input)))


;;      *
;;     / \
;;    *   4
;;   /|\
;;  1 2 *
;;      |
;;      3
(map first (tree-seq next rest '((1 2 (3)) (4))))
(map first (tree-seq next rest '((1 2 (3)) (4)))
;; => ((1 2 (3)) (4))
;; => ((1 2 (3)) 4)
;;((1 2 (3)) (4))
;;
;;(      *      )
;; ( *     ) (*)
;;  1 2 (*)   4
;;       3
