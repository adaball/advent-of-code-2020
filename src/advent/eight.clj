(ns advent.eight
  (:require [clojure.string :refer [split]]))

(defn get-input []
  (let [raw          (-> (slurp "resources/eight-fixed.csv")
                         (split #"\n"))
        instructions (map #(split % #" ") raw)]
    (vec instructions)))

(def run-operations (atom #{}))
(def pc (atom 0))
(def acc-v (atom 0))
(def should-log (atom false))

(defn log [msg]
  (if @should-log
    (print msg)))

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
  (log (format "RUN: pc: %d\tacc-v: %d\toperation: %s\toperand: %s\n" (inc @pc) @acc-v operation operand))
  (cond
    (= operation "nop") (nop operand)
    (= operation "acc") (acc operand)
    (= operation "jmp") (jmp operand)
    :else (throw (IllegalStateException. (str "invalid operation: " operation)))))

(defn run-program [input]
  (reset! should-log false)
  (reset! pc 0)
  (reset! run-operations #{})
  (reset! acc-v 0)
  (let [ins (get input @pc)]
    (loop [ins ins]
      (log (format "LOOP START: ins %s\n" ins))
      (let [operation (first ins)
            operand   (second ins)]
        (if (or (operation-run? @pc operation operand) (nil? operation))
          {:acc-v @acc-v :operation operation :operand operand :pc @pc}
          (do (run operation operand)
              (recur (get input @pc))))))))

(defn solve-one []
  (run-program (get-input)))

(defn flip-jmp-or-nop [ins-vec idx]
  (let [ins       (nth ins-vec idx)
        operation (nth ins 1)
        operand   (nth ins 2)]
    (if (not (or (= "jmp" operation) (= "nop" operation)))
      nil
      (let [new-op (if (= "jmp" operation) "nop" "jmp")]
        (replace {[idx operation operand] [idx new-op operand]} ins-vec)))))

(defn add-idx-to-ins [ins-vec]
  (map (fn [idx [operation operand]] [idx operation operand])
       (range 0 (count ins-vec))
       ins-vec))

(defn remove-idx-from-ins [ins-vec]
  (map (fn [[idx operation operand]] [operation operand]) ins-vec))

(defn solve-two []
  (let [ins-vec   (get-input)
        ins-w-idx (add-idx-to-ins ins-vec)]
    (->> (range 0 (count ins-vec))
         (map #(flip-jmp-or-nop ins-w-idx %))
         (filter #(not (nil? %)))
         (map remove-idx-from-ins)
         (map vec)
         (map run-program)
         (filter #(nil? (:operation %))))))
