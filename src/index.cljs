(ns index
  (:require [animation :as animation]))

(def state (animation/initialize-state animation/default-settings))

(def background-element (.getElementById js/document "background-canvas"))

(defn render-background []
  (let [time-seconds (-> (js/Date.) .getTime (* 0.001))
        polyhedron (animation/polyhedron-from-state state time-seconds)
        width (.-clientWidth background-element)
        height (.-clientHeight background-element)
        size [width height]
        fit (animation/fit-camera size (:bounding-box state))
        proj (animation/projector fit)
        planes-to-render (into []
                               (keep (fn [plane-data]
                                       (let [[_ _ z] (-> plane-data :plane :normal)]
                                         (when (< 0 z)
                                           plane-data))))
                               (:planes polyhedron))
        ctx (.getContext background-element "2d")]
    (set! (.-width background-element) width)
    (set! (.-height background-element) height)
    (.clearRect ctx 0 0 width height)
    (doseq [{:keys [corner-loop]} planes-to-render
            :when (<= 3 (count corner-loop))
            :let [[start-corner & rest-corners] corner-loop]]
      (.beginPath ctx)
      (let [[x y] (proj (:position start-corner))]
        (.moveTo ctx x y))
      (doseq [corner rest-corners
              :let [[x y] (proj (:position corner))]]
        (.lineTo ctx x y))
      (.closePath ctx)
      (.stroke ctx))))

(js/setInterval render-background 30)
;;(render-background)


