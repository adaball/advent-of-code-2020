(ns advent.three)

(defn get-input []
  (clojure.string/split (slurp "resources/three.txt") #"\n"))

(defn get-val-at-coords [slope-map x y]
  (if (> y (dec (count slope-map)))
    nil
    (let [row (nth slope-map y)]
      (if (> x (dec (count row)))
        nil
        (nth row x)))))

(defn get-wrapped-coords [map-width input-x input-y]
  (loop [mw map-width
         x  input-x
         y  input-y]
    (if (< x mw)
      '(x y)
      (recur mw (- x mw) y))))

(defn get-next-coords [map-width input-x input-y right down]
  (let [[curr-x curr-y] (get-wrapped-coords map-width input-x input-y)]
    (get-wrapped-coords map-width (+ right curr-x) (+ down curr-y))))

(defn gen-path [slope-map right down]
  (let [map-width (count (first slope-map))]
    (loop [x   0
           y   0
           res '()]
      (let [cur-val (get-val-at-coords slope-map x y)
            [n-x n-y]   (get-next-coords map-width x y right down)]
        (if (nil? cur-val)
          res
          (recur n-x n-y (conj res cur-val)))))))

(defn solve-one []
  (count (filter #(= \# %) (gen-path (get-input) 3 1))))

(defn solve-two []
  (let [slope-map (get-input)
        p-one     (gen-path slope-map 1 1)
        p-two     (gen-path slope-map 3 1)
        p-three   (gen-path slope-map 5 1)
        p-four    (gen-path slope-map 7 1)
        p-five    (gen-path slope-map 1 2)
        cnt-fn    (fn [path] (count (filter #(= \# %) path)))
        totals    (map cnt-fn (list p-one p-two p-three p-four p-five))]
    (apply * totals)))
