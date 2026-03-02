(ns index
  (:require [animation :as animation]))

(def state (animation/initialize-state animation/default-settings))

(def background-element (.getElementById js/document "background-canvas"))

(defn render-background []
  (let [time-seconds (-> (js/Date.) .getTime (* 0.001))
        polyhedron (animation/polyhedron-from-state state time-seconds)
        width (.-width background-element)
        height (.-height background-element)
        ]
    (set! (.-width background-element) (.-clientWidth background-element))
    (set! (.-height background-element) (.-clientHeight background-element))
    (println "Polyhedron" [width height])))

(js/setInterval render-background 30)
