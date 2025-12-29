(ns animation-test
  (:require [animation :as animation]
            [linalg :as linalg]
            ["vitest" :refer [expect test]]))

(defn near? [a b tol]
  (= (Math/round (/ a tol))
     (Math/round (/ b tol))))

(test "animation"
      (-> (expect (animation/bounding-box [[1 2]
                                           [10 20]
                                           [3 -4]]))
          (.toEqual [{:lower 1 :upper 10}
                     {:lower -4 :upper 20}])))

(test "fit-camera-test"
      (let [pts [[1 1 3]
                 [10 13 7]]
            bdbox (animation/bounding-box pts)
            size [640 480]
            cam (animation/fit-camera size bdbox)
            proj (animation/projector cam)]
        (-> (expect (proj [1 1 3]))
            (.toEqual [140 0 3]))
        (-> (expect (proj [10 13 3 50]))
            (.toEqual [500 480 3 50]))))

(test "random-unit-vector-test"
      (do (let [v (animation/random-vector-3d)]
            (-> (expect (count v))
                (.toBe 3))
            (-> (expect (<= (linalg/squared-norm v) 1))
                (.toBe true)))
          (let [v (animation/random-unit-vector-3d
                                 animation/random-vector-3d)]
            (-> (expect (count v))
                (.toBe 3))
            (-> (expect (near? (linalg/squared-norm v)
                               1.0
                               1.0e-6))
                (.toBe true)))))

(test "random-subspace-test"
      (let [[x y] (animation/random-subspace)]
        (-> (expect (count x))
            (.toBe 3))
        (-> (expect (count y))
            (.toBe 3))
        (-> (expect (near? (linalg/squared-norm x) 1 1.0e-6))
            (.toBe true))
        (-> (expect (near? (linalg/squared-norm y) 1 1.0e-6))
            (.toBe true))
        (-> (expect (near? (linalg/dot-product x y) 0.0 1.0e-6))
            (.toBe true))))

(test "initialize-state-test"
      (let [settings animation/default-settings
            {:keys [planes bounding-box] :as state}
            (animation/initialize-state settings)
            ]
        (-> (expect (boolean bounding-box))
            (.toBe true))
        (-> (expect (boolean (seq planes)))
            (.toBe true))
        (-> (expect (count planes))
            (.toBe (:plane-count settings)))
        (dotimes [i 20]
          (let [polyhedron (animation/polyhedron-from-state state i)]
            (-> (expect (linalg/polyhedron? polyhedron))
                (.toBe true))
            (-> (expect (-> polyhedron :planes seq boolean))
                (.toBe true))))))
