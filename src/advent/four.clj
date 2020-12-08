(ns advent.four
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [split lower-case]]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})
(def valid-ecl-vals #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn get-input []
  (->> (split (slurp "resources/four.txt") #"\n{2,}")
       (map #(split % #"\n"))
       (map (fn [parts] (map #(split % #" ") parts)))
       (map flatten)))

(defn get-fields [passport]
  (let [fields (map #(first (split % #":")) passport)]
    (set fields)))

(defn req-fields-present? [passport]
  (empty? (difference required-fields (get-fields passport))))

(defn solve-one []
  (count (filter true? (map req-fields-present? (get-input)))))

(defn four-digit-validation [v]
  (not (nil? (re-matches #"\d{4}" v))))

(defn year-validation [v lower upper] 
  (if (four-digit-validation v)
    (let [n (Integer/parseInt v)]
      (and (>= n lower) (<= n upper)))
    nil))

(defn byr-validation [v]
  (year-validation v 1920 2002))

(defn ecl-validation [v]
  (empty? (difference #{v} valid-ecl-vals)))

(defn eyr-validation [v]
  (year-validation v 2020 2030))

(defn hcl-validation [v]
  (not (nil? (re-matches #"#[a-z0-9]{6}" v))))

(defn hgt-validation [v]
  (let [m (re-matches #"(\d+)(cm|CM|Cm|cM|in|IN|In|iN)" v)]
    (if (not= 3 (count m))
      nil
      (let [amt  (Integer/parseInt (nth m 1))
            unit (lower-case (nth m 2))]
        (cond
          (= "cm" unit) (and (>= amt 150) (<= amt 193))
          (= "in" unit) (and (>= amt 59) (<= amt 76))
          :else nil)))))

(defn iyr-validation [v]
  (year-validation v 2010 2020))

(defn pid-validation [v]
  (not (nil? (re-matches #"[0-9]{9}" v))))

(defn validate-value [t v]
  (cond
    (= t "byr") (byr-validation v)
    (= t "cid") true
    (= t "ecl") (ecl-validation v)
    (= t "eyr") (eyr-validation v)
    (= t "hcl") (hcl-validation v)
    (= t "hgt") (hgt-validation v)
    (= t "iyr") (iyr-validation v)
    (= t "pid") (pid-validation v)
    :else nil))

(defn validate-passport [passport]
  (if (not (req-fields-present? passport))
    nil
    (let [kv-coll (map #(split % #":") passport)
          kv-coll (map #(apply validate-value %) kv-coll)]
      (every? true? kv-coll))))

(defn solve-two []
  (let [passports (get-input)]
    (count (filter true? (map validate-passport passports)))))
