(ns polyhedron-test
  (:require [polyhedron :as polyhedron]
            ["vitest" :refer [expect test]]))

(test "dot-product"
      (do 
        (-> (expect (polyhedron/dot-product [3 4 5] [10 100 1000]))
            (.toBe 5430))
        (-> (expect (polyhedron/dot-product [2 0] [0 2]))
            (.toBe 0))))

(test "plane-test"
      (let [plane (polyhedron/plane-with-normal-at-point [1 0 0]
                                                        [3 3 3])]
        (-> (expect (polyhedron/evaluate-plane plane [3 0 0]))
            (.toBe 0))
        (-> (expect (polyhedron/evaluate-plane plane [4 0 0]))
            (.toBe 1))
        (-> (expect (polyhedron/evaluate-plane plane [4 20 20]))
            (.toBe 1))
        (-> (expect (polyhedron/point-on-plane plane))
            (.toEqual [3 0 0]))))

(test "cross-product-test"
      (do 
        (-> (expect (polyhedron/cross-product [1 0 0] [0 1 0]))
            (.toEqual [0 0 1]))
        (-> (expect (polyhedron/cross-product [0 1 0] [1 0 0]))
            (.toEqual [0 0 -1]))
        (-> (expect (polyhedron/cross-product [0 1 0] [0 0 1]))
            (.toEqual [1 0 0]))))

(test "intersect-planes-test"
      (let [plane-a (polyhedron/plane-with-normal-at-point
                     [3 0 0] [4 9 17])
            plane-b (polyhedron/plane-with-normal-at-point
                     [0 7 0] [5 55 10])
            intersection (polyhedron/intersect-planes
                          plane-a plane-b
                          (:tol polyhedron/default-settings))
            {:keys [direction point]} intersection]
        (-> (expect direction)
            (.toEqual [0 0 21]))
        (-> (expect point)
            (.toEqual [4 55 0]))))

(test "order-corner-loop-test"
      (-> (expect (polyhedron/order-corner-loop
                   ["r/h" "z/h" "z/k" "k/u" "u/r"]))
          (.toEqual ["k/u" "u/r" "r/h" "z/h" "z/k"])))


(test "polyhedron-test"
      (do (let [{:keys [planes]} (polyhedron/polyhedron
                                  {:a (polyhedron/plane [1 0 0] 0)
                                   :b (polyhedron/plane [0 1 0] 0)
                                   :c (polyhedron/plane [0 0 1] 0)
                                   :d (polyhedron/plane-with-normal-at-point
                                       [-1 -1 -1]
                                       [1 0 0])}
                                  (:tol polyhedron/default-settings))
                [a _b _c _d] planes]
            (-> (expect (count planes))
                (.toEqual 4))
            (-> (expect (-> a :corner-loop))
                (.toEqual [{:key ["b" "c"] :position [0 0 0]}
                           {:key ["b" "d"] :position [0 0 1]}
                           {:key ["c" "d"] :position [0 1 0]}]))
            (-> (expect (:key a))
                (.toEqual "a"))
            (-> (expect (:plane a))
                (.toEqual (polyhedron/plane [1 0 0] 0))))
          (-> (expect (polyhedron/polyhedron
                       {:a (polyhedron/plane [1 0 0] 0)}
                       (:tol polyhedron/default-settings)))
              (.toEqual {:corners {} :planes []}))
          (let [tol (:tol polyhedron/default-settings)
                x (polyhedron/polyhedron
                   {:a (polyhedron/plane [1 0 0] 0)
                    :b (polyhedron/plane [0 1 0] 0)
                    :c (polyhedron/plane [0 0 1] 0)
                    :d (polyhedron/plane-with-normal-at-point
                        [-1 -1 -1]
                        [1 0 0])}
                   tol)
                y (polyhedron/polyhedron
                   {:a (polyhedron/plane [1 0 0] 0)
                    :q (polyhedron/plane [1 0 0] -10)
                    :b (polyhedron/plane [0 1 0] 0)
                    :c (polyhedron/plane [0 0 1] 0)
                    :d (polyhedron/plane-with-normal-at-point
                        [-1 -1 -1]
                        [1 0 0])}
                   tol)]
            (-> (expect x)
                (.toEqual y)))))

