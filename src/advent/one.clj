(ns advent.one)

(defn get-input []
  (map #(Integer/parseInt %)
    (clojure.string/split (slurp "resources/one.csv") #"\n")))

(defn inner-reduce-one [coll x]
  (reduce (fn [acc v]
            (if (not (nil? acc))
              acc
              (if (and (not= x v) (= (+ x v) 2020))
                (* x v)
                nil)))
          nil 
          coll))

(defn solve-one []
  (let [input   (get-input)
        raw-res (map (fn [x] (inner-reduce-one input x)) input)
        res     (first (filter #(not (nil? %)) raw-res))]
    res))

(defn get-next [cnt t a b c]
  (let [c (if (= (- cnt 1) c) 
            0 
            (inc c))
        b (if (= 0 c) 
            (if (= (- cnt 1) b) 0 (inc b)) 
            b)
        a (if (and (= 0 b) (= 0 c)) 
            (if (= (- cnt 1) a) nil (inc a)) 
            a)]
    (cond
      (= t 'a) a
      (= t 'b) b
      (= t 'c) c)))

(defn solve-two []
  (let [input (get-input)
        cnt   (count input)]
    (loop [a 0 b 1 c 2]
      (let [a-val (if (nil? a) a (nth input a))
            b-val (nth input b)
            c-val (nth input c)]
        (if (or (= (+ a-val b-val c-val) 2020) (nil? a-val))
          (if (nil? a-val)
            nil
            (* a-val b-val c-val))
          (recur (get-next cnt 'a a b c)
                 (get-next cnt 'b a b c)
                 (get-next cnt 'c a b c)))))))
