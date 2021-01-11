(ns advent.eight
  (:require [clojure.string :refer [split]]))

(defn get-input []
  (let [raw          (-> (slurp "resources/eight.csv")
                         (split #"\n"))
        instructions (map #(split % #" ") raw)]
    (vec instructions)))

(def run-operations (atom #{}))
(def pc (atom 0))
(def acc-v (atom 0))

(defn operation-run? [pc operation operand]
  (contains? @run-operations (str pc operation operand)))

(defn jmp [operand]
  (swap! run-operations conj (str @pc "jmp" operand))
  (swap! pc + (Integer/parseInt operand)))

(defn acc [operand]
  (swap! acc-v + (Integer/parseInt operand))
  (swap! run-operations conj (str @pc "acc" operand))
  (swap! pc inc))

(defn nop [operand]
  (swap! run-operations conj (str @pc "nop" operand))
  (swap! pc inc))

(defn run [operation operand]
  (print (format "RUN: pc: %d\tacc-v: %d\toperation: %s\toperand: %s\n" @pc @acc-v operation operand))
  (cond
    (= operation "nop") (nop operand)
    (= operation "acc") (acc operand)
    (= operation "jmp") (jmp operand)
    :else (throw (IllegalStateException. (str "invalid operation: " operation)))))

(defn solve-one []
  (reset! pc 0)
  (reset! run-operations #{})
  (reset! acc-v 0)
  (let [instructions (get-input)
        ins          (get instructions @pc)]
    (loop [ins ins]
      (let [operation (first ins)
            operand   (second ins)]
        (if (operation-run? @pc operation operand)
          @acc-v
          (do (run operation operand)
              (recur (get instructions @pc))))))))
