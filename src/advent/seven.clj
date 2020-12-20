(ns advent.seven
  (:require [clojure.string :refer [split]]))

(def bag-regex #"([\d]*[a-z ]+) bags?")

(defn get-input []
  (let [raw-text (slurp "resources/seven.csv")
        lines    (split raw-text #"\n")
        lines    (map #(split % #",\s?") lines)]
    lines))
