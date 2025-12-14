(ns animation-test
  (:require [animation :as animation]
            ["vitest" :refer [expect test]]))

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
