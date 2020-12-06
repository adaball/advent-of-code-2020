(ns advent.two
  (:require [clojure.string :refer [split]]))

(defn get-lower-upper [s]
  (let [both  (split s #"-")
        lower (Integer/parseInt (first both))
        upper (Integer/parseInt (second both))]
    (list lower upper)))

(defn get-input []
  (let [file-str (slurp "resources/two.csv")
        vec-strs (map #(split % #" ") (split file-str #"\n"))]
    (map (fn [v]
           (let [[l u] (get-lower-upper (first v))
                 req-k (first (split (second v) #":"))
                 pass  (nth v 2)]
             {:lower l
              :upper u
              :required-key req-k
              :password pass}))
         vec-strs)))

(defn get-letter-counts [pass]
  (let [letters (split pass #"")
        cnt-fn  (fn [acc v]
                  (if-let [cnt (get acc v)]
                    (assoc acc v (inc cnt))
                    (assoc acc v 1)))]
    (reduce cnt-fn {} letters)))

(defn validate-password-one [password-entry]
  (let [l       (:lower password-entry)
        u       (:upper password-entry)
        req-k   (:required-key password-entry)
        pass    (:password password-entry)
        l-cnts  (get-letter-counts pass)
        req-cnt (if-let [v (get l-cnts req-k)] v 0)]
    (and (>= req-cnt l) (<= req-cnt u))))

(defn solve-one []
  (let [input (get-input)]
    (count (filter true? (map validate-password-one input)))))

(defn validate-password-two [password-entry]
  (let [idx-one    (dec (:lower password-entry))
        idx-two    (dec (:upper password-entry))
        req-k      (:required-key password-entry)
        req-k-char (first (.toCharArray req-k))
        pass       (:password password-entry)
        val-one    (get pass idx-one)
        val-two    (get pass idx-two)]
    (or (and (= val-one req-k-char) (not= val-two req-k-char))
        (and (= val-two req-k-char) (not= val-one req-k-char)))))

(defn solve-two []
  (let [input (get-input)]
    (count (filter true? (map validate-password-two input)))))

