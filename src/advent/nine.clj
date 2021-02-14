(ns advent.nine)

(def preamble-len 25)

(defn get-input
  []
  (let [raw (clojure.string/split (slurp "resources/nine.csv") #"\n")]
    (->> raw
         (map #(Long/parseLong %))
         vec)))

(defn sub-vec
  [v start-idx end-idx]
  (if (or (< end-idx start-idx) (>= (- end-idx start-idx) (count v)))
    (throw (IllegalArgumentException.
            (format "bad vals for sub-vec idx, start: %d end: %d"
                    start-idx
                    end-idx))))
  (->> (range start-idx (inc end-idx))
       (map #(nth v %))
       vec))

(defn process-number [n prev] {:num n, :prev (set prev), :valid? nil})

(defn process-numbers
  [input start-idx]
  "given the input vector, get the [preamble-len] previous numbers for the given index 
  (transformed via process-number)"
  (let [start-idx preamble-len]
    (map #(process-number (nth input %)
                          (sub-vec input (- % preamble-len) (dec %)))
         (range start-idx (count input)))))

(defn validate-number
  [m]
  (let [a 0
        b 1
        v (vec (:prev m))
        len (count v)
        n (:num m)]
    (loop [a a
           b b]
      (if (and (not= len a) (not= a b) (= n (+ (nth v a) (nth v b))))
        true
        (if (= len a) false (recur (inc a) (if (= b (dec len)) 0 (inc b))))))))

; guessed 20, that was wrong
(defn solve-one
  []
  (let [processed-numbers (process-numbers (get-input) preamble-len)]
    (->> processed-numbers
         (map #(merge % {:valid (validate-number %)}))
         (filter #(true? (:valid %)))
         first
         :num)))
