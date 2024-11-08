(ns brian-test-2.core
  (:gen-class)
  (:require
    [hyperfiddle.rcf :refer [tests]]
    [missionary.core :as m]))


(defn flow-of-flows

  "Original function from task description"

  []
  (m/ap (let [flow (m/?> ##Inf (m/seed (range 100)))]
          (m/seed (range flow)))))


(defn math

  "Strict mathematical solution. Out of competition.

  Pros: fastest, complexity O(1), known since ancient times
  Cons: it's easy to get confused, not a Missionary"

  [n]
  (let [k (dec n)]
    (* (/ 1 2)
       (- (/ (* k (inc k) (inc (* 2 k)))
             6)
          (/ (* k (inc k))
             2)))))


(defn direct-forks

  "Most straightforward way.

  Pros: intuitive
  Cons: slowest of all, I guess such forks not use all benefits
        of parallelism or opposite have too much overhead on every fork"

  []
  (m/? (m/reduce + (m/ap (m/?> (m/?> (flow-of-flows)))))))


(defn transducer-add

  "Transducing outer flow summarising inner ones.

  Pros: one of fastest
  Cons: no... maybe a lack of elegance"

  []
  (let [xf (map #(m/? (m/reduce + %)))]
    (m/? (->> (flow-of-flows)
              (m/eduction xf)
              (m/reduce +)))))


(defn transducer-conj

  "Transducing outer flow by flattening it

  Pros: good option if you need all elements of all flows at once
  Cons: slow, I guess overhead of aux sequences creation"

  []
  (let [xf (comp (map #(m/? (m/reduce conj %))) cat)]
    (m/? (->> (flow-of-flows)
              (m/eduction xf)
              (m/reduce +)))))


(defn reducer

  "Reducing outer flow by summarising inner ones

  Pros: ties for name of fastest with `transducer-add`, same logic
        under the hood
  Cons: looks bit more complicated"

  []
  (let [rf #(+ %1 (m/? (m/reduce + %2)))]
    (m/? (->> (flow-of-flows)
              (m/reduce rf 0)))))


(hyperfiddle.rcf/enable!)


(tests
  "best"
  (direct-forks) := (math 100)

  "basic"
  (transducer-add) := (math 100)

  "transducer"
  (transducer-conj) := (math 100)

  "reducer"
  (reducer) := (math 100))


(time (doseq [_ (range 1000)]
        (math 100)))           ; ~0.7 msecs (Out of competition)


(time (doseq [_ (range 1000)]
        (direct-forks)))       ; ~390 msecs


(time (doseq [_ (range 1000)]
        (transducer-add)))     ; ~95 msecs


(time (doseq [_ (range 1000)]
        (transducer-conj)))    ; ~208 msecs


(time (doseq [_ (range 1000)]
        (reducer)))            ; ~95 msecs
