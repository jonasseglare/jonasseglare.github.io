(ns linalg-test
  (:require [linalg :as linalg]
            ["vitest" :refer [expect test]]))

(test "dot-product"
      (do 
        (-> (expect (linalg/dot-product [3 4 5] [10 100 1000]))
            (.toBe 5430))
        (-> (expect (linalg/dot-product [2 0] [0 2]))
            (.toBe 0))))

(test "norm-test"
      (do (-> (expect (linalg/squared-norm [3 4]))
              (.toBe 25))
          (-> (expect (linalg/norm [3 4]))
              (.toBe 5))))

(test "normalize-test"
      (do (-> (expect (linalg/normalize [3 0]))
              (.toEqual [1 0]))
          (-> (expect (linalg/normalize-or-nil [3 0]))
              (.toEqual [1 0]))
          (-> (expect (nil? (linalg/normalize-or-nil [0 0])))
              (.toBe true))))

(test "orthogonalize"
      (do (-> (expect (linalg/orthogonalize [9 7] [1 0]))
              (.toEqual [0 7]))))

(test "plane-test"
      (let [plane (linalg/plane-with-normal-at-point [1 0 0]
                                                     [3 3 3])]
        (-> (expect (linalg/evaluate-plane plane [3 0 0]))
            (.toBe 0))
        (-> (expect (linalg/evaluate-plane plane [4 0 0]))
            (.toBe 1))
        (-> (expect (linalg/evaluate-plane plane [4 20 20]))
            (.toBe 1))
        (-> (expect (linalg/point-on-plane plane))
            (.toEqual [3 0 0]))))

(test "cross-product-test"
      (do 
        (-> (expect (linalg/cross-product [1 0 0] [0 1 0]))
            (.toEqual [0 0 1]))
        (-> (expect (linalg/cross-product [0 1 0] [1 0 0]))
            (.toEqual [0 0 -1]))
        (-> (expect (linalg/cross-product [0 1 0] [0 0 1]))
            (.toEqual [1 0 0]))))

(test "intersect-planes-test"
      (let [plane-a (linalg/plane-with-normal-at-point
                     [3 0 0] [4 9 17])
            plane-b (linalg/plane-with-normal-at-point
                     [0 7 0] [5 55 10])
            intersection (linalg/intersect-planes
                          plane-a plane-b
                          (:tol linalg/default-settings))
            {:keys [direction point]} intersection]
        (-> (expect direction)
            (.toEqual [0 0 21]))
        (-> (expect point)
            (.toEqual [4 55 0]))))

(test "order-corner-loop-test"
      (-> (expect (linalg/order-corner-loop
                   ["r/h" "z/h" "z/k" "k/u" "u/r"]))
          (.toEqual ["k/u" "u/r" "r/h" "z/h" "z/k"])))


(test "polyhedron-test"
      (do (let [{:keys [planes]} (linalg/polyhedron
                                  {:a (linalg/plane [1 0 0] 0)
                                   :b (linalg/plane [0 1 0] 0)
                                   :c (linalg/plane [0 0 1] 0)
                                   :d (linalg/plane-with-normal-at-point
                                       [-1 -1 -1]
                                       [1 0 0])}
                                  (:tol linalg/default-settings))
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
                (.toEqual (linalg/plane [1 0 0] 0))))
          (-> (expect (linalg/polyhedron
                       {:a (linalg/plane [1 0 0] 0)}
                       (:tol linalg/default-settings)))
              (.toEqual {:corners {} :planes []}))
          (let [tol (:tol linalg/default-settings)
                x (linalg/polyhedron
                   {:a (linalg/plane [1 0 0] 0)
                    :b (linalg/plane [0 1 0] 0)
                    :c (linalg/plane [0 0 1] 0)
                    :d (linalg/plane-with-normal-at-point
                        [-1 -1 -1]
                        [1 0 0])}
                   tol)
                y (linalg/polyhedron
                   {:a (linalg/plane [1 0 0] 0)
                    :q (linalg/plane [1 0 0] -10)
                    :b (linalg/plane [0 1 0] 0)
                    :c (linalg/plane [0 0 1] 0)
                    :d (linalg/plane-with-normal-at-point
                        [-1 -1 -1]
                        [1 0 0])}
                   tol)]
            (-> (expect x)
                (.toEqual y)))))

