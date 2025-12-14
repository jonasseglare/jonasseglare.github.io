(ns animation-test
  (:require [animation :as animation]
            ["vitest" :refer [expect test]]))

(test "dot-product"
      (do 
        (-> (expect (animation/dot-product [3 4 5] [10 100 1000]))
            (.toBe 5430))
        (-> (expect (animation/dot-product [2 0] [0 2]))
            (.toBe 0))))

(test "plane-test"
      (let [plane (animation/plane-with-normal-at-point [1 0 0]
                                                        [3 3 3])]
        (-> (expect (animation/evaluate-plane plane [3 0 0]))
            (.toBe 0))
        (-> (expect (animation/evaluate-plane plane [4 0 0]))
            (.toBe 1))
        (-> (expect (animation/evaluate-plane plane [4 20 20]))
            (.toBe 1))
        (-> (expect (animation/point-on-plane plane))
            (.toEqual [3 0 0]))))

(test "cross-product-test"
      (do 
        (-> (expect (animation/cross-product [1 0 0] [0 1 0]))
            (.toEqual [0 0 1]))
        (-> (expect (animation/cross-product [0 1 0] [1 0 0]))
            (.toEqual [0 0 -1]))
        (-> (expect (animation/cross-product [0 1 0] [0 0 1]))
            (.toEqual [1 0 0]))))

(test "intersect-planes-test"
      (let [plane-a (animation/plane-with-normal-at-point
                     [3 0 0] [4 9 17])
            plane-b (animation/plane-with-normal-at-point
                     [0 7 0] [5 55 10])
            intersection (animation/intersect-planes
                          plane-a plane-b
                          animation/default-settings)
            {:keys [direction point]} intersection]
        (-> (expect direction)
            (.toEqual [0 0 21]))
        (-> (expect point)
            (.toEqual [4 55 0]))))
