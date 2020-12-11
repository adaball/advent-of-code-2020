(ns advent.five
  (:require [clojure.string :refer [split]]))

(defn get-input []
  (let [lines  (split (slurp "resources/five.csv") #"\n")
        lines  (map #(split % #"") lines)
        col-fn (fn [coll] (map #(nth coll %) (range 0 7)))
        row-fn (fn [coll] (map #(nth coll %) (range 7 10)))]
    (map #(list (col-fn %) (row-fn %)) lines)))

(defn get-seat-id [row column]
  (+ column (* row 8)))

(defn get-coords [t l u]
  (if (= 1 (- u l))
    (if (or (= t "F") (= t "L")) l u)
    (let [half (float (/ (- u l) 2))
          half (if (or (= t "F") (= t "L"))
                 (Math/floor (- u half))
                 (Math/ceil (+ l half)))]
      (if (or (= t "F") (= t "L"))
        (list (int l) (int half))
        (list (int half) (int u))))))

(defn find-coords [l u input]
  (loop [l l u u input input]
    (let [res (get-coords (first input) l u)]
      (if (or (not (list? res)) (empty? input))
        res
        (recur (first res) (second res) (rest input))))))

(defn solve-one []
  (let [input (get-input)
        res   (map #(list (find-coords 0 127 (first %))
                          (find-coords 0 7 (second %)))
                   input)
        res   (map #(get-seat-id (first %) (second %)) res)]
    (apply max res)))

(defn find-seat [seat-ids]
  (let [seat-ids (sort seat-ids)
        cnt      (count seat-ids)]
    (loop [x 0 y 1 ans nil]
      (if (or (>= x (dec cnt)) (>= y (dec cnt)) (not (nil? ans)))
        ans
        (recur (inc x)
               (inc y)
               (if (= 2 (- (nth seat-ids y) (nth seat-ids x)))
                 (inc (nth seat-ids x))
                 nil))))))

(defn solve-two []
  (let [input (get-input)
        res   (map #(list (find-coords 0 127 (first %))
                          (find-coords 0 7 (second %)))
                   input)
        res   (map #(get-seat-id (first %) (second %)) res)]
    (find-seat res)))
