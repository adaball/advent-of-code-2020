(ns advent.six
  (:require [clojure.string :refer [split]]
            [clojure.set :refer [intersection]]))

(defn get-input []
  (->> (split (slurp "resources/six.csv") #"\n{2,}")
       (map #(split % #"\n"))))

(defn get-group-unique [group]
  (reduce (fn [acc v]
            (into acc (set v)))
          #{}
          group))

(defn get-groups-unique [groups]
  (map get-group-unique groups))

(defn solve-one []
  (let [input (get-input)]
    (apply + (map count (get-groups-unique input)))))

(defn get-groups-all-yes [groups]
  (let [groups (map #(map set %) groups)]
    (map (fn [sets] (apply intersection sets)) groups)))

(defn solve-two []
  (let [input   (get-input)
        all-yes (get-groups-all-yes input)]
    (apply + (map count all-yes))))
