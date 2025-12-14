(ns animation-test
  (:require [animation :as animation]
            ["vitest" :refer [expect test]]))

(test "animation"
      (println (animation/bounding-box [[1 2]
                                        [10 20]
                                        [3 -4]])))
