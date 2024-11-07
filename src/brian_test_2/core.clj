(ns brian-test-2.core
  (:gen-class)
  (:require
    [hyperfiddle.rcf :refer [tests tap %]]
    [missionary.core :as m]))


(defn flow-of-flows

  "Original function from task description"

  []
  (m/ap (let [flow (m/?> ##Inf (m/seed (range 100)))]
          (m/seed (range flow)))))


(defn math

  "Strict mathematical solution.

  Pros: fastest, complexity O(1), known from ancient times
  Cons: it's easy to get confused, not a missionary"

  [n]
  (let [k (dec n)]
    (* (/ 1 2)
       (- (/ (* k (inc k) (inc (* 2 k)))
             6)
          (/ (* k (inc k))
             2)))))


(defn best
  []
  (m/? (m/ap (loop [s 0]
               (let [f (m/?> ##Inf (flow-of-flows))]
                 (m/amb s
                        (recur (m/reduce + s f))))))))


(best)


(defn basic
  []
  (m/? (->> (flow-of-flows)
            (m/eduction (map #(m/? (m/reduce + %))))
            (m/reduce +))))


(defn transducer
  []
  (let [xf (comp (map #(m/? (m/reduce conj %))) cat)]
    (m/? (->> (flow-of-flows)
              (m/eduction xf)
              (m/reduce +)))))


(defn reducer
  []
  (let [rf #(+ %1 (m/? (m/reduce + %2)))]
    (m/? (->> (flow-of-flows)
              (m/reduce rf 0)))))


(hyperfiddle.rcf/enable!)


(tests
  "best"
  (best) := (math 100)

  "basic"
  (basic) := (math 100)

  "transducer"
  (transducer) := (math 100)

  "reducer"
  (reducer) := (math 100))
