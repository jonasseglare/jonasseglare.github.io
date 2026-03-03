(ns index
  (:require [animation :as animation]
            [linalg :as linalg]))

(def settings {:animation animation/default-settings
               :render-interval-ms 30
               :light-vector (linalg/normalize [-1 1 1])
               :diffuse-amount 0.5
               :ambient-amount 0.25
               :main-color [0 171 255] ;; hue 200
               :odd-color [255 0 170] 
               :saturation 0.5
               :background-color [0 0 0]})

(defn saturate [[r g b] weight]
  (let [avg (/ (+ r g b) 3)
        counter-weight (- 1.0 weight)
        sat (fn [x] (+ (* weight x) (* counter-weight avg)))]
    [(sat r) (sat g) (sat b)]))

(def state (-> settings :animation animation/initialize-state))

(def background-element (.getElementById js/document "background-canvas"))

(defn compute-light-intensity [normal {:keys [light-vector
                                              diffuse-amount
                                              ambient-amount]}]
  (->> (linalg/dot-product light-vector normal)
       (Math/max 0.0)
       (* diffuse-amount)
       (+ ambient-amount)))

(defn rgb-expr [[r g b]]
  (str "rgb(" (Math/round r) ", " (Math/round g) ", " (Math/round b) ")"))


(defn select-odd-plane [[k visible?] polyhedron visible-ks]
  (let [visible-ks (set visible-ks)
        invisible-ks (into #{}
                           (comp (map :key)
                                 (remove visible-ks))
                           (:planes polyhedron))]
    
    (if (or (not k) (and visible? (contains? invisible-ks k)))
      [(rand-nth invisible-ks) false]
      [k (contains? visible-ks k)])))

(def odd-plane-state (atom nil))

(defn process-corner-positions [positions]
  positions)

(defn render-background [settings]
  (let [time-seconds (-> (js/Date.) .getTime (* 0.001))
        polyhedron (animation/polyhedron-from-state state time-seconds)
        ;;_ (println polyhedron)
        width (.-clientWidth background-element)
        height (.-clientHeight background-element)
        size [width height]
        fit (animation/fit-camera size (:bounding-box state))
        proj (animation/projector fit)
        planes-to-render (into []
                               (keep (fn [plane-data]
                                       (let [[_ _ z :as normal] (-> plane-data
                                                                    :plane
                                                                    :normal)]
                                         (when (< 0 z)
                                           (assoc plane-data
                                                  :light-intensity
                                                  (compute-light-intensity normal
                                                                           settings))))))
                               (:planes polyhedron))
        visible-ks (into #{} (map :key) planes-to-render)
        [odd-key] (swap! odd-plane-state select-odd-plane polyhedron visible-ks)
        ctx (.getContext background-element "2d")]
    (println odd-key)
    (set! (.-width background-element) width)
    (set! (.-height background-element) height)
    (set! (.-fillStyle ctx) (rgb-expr (:background-color settings)))
    (set! (.-lineWidth ctx) 0)
    (.fillRect ctx 0 0 width height)
    (set! (.-filter ctx) "blur(7px)")
    ;;myContext.filter = 'blur(10px)';
    (doseq [{:keys [corner-loop light-intensity key]} planes-to-render
            :when (<= 3 (count corner-loop))
            :let [[start-pos & rest-pos] (process-corner-positions (mapv (comp proj :position) corner-loop))
                  base-color (get settings
                                  (if (= key odd-key) :odd-color :main-color))
                  color (-> light-intensity
                            (linalg/scale-vector base-color)
                            (saturate (:saturation settings)))]]
      (set! (.-fillStyle ctx) (rgb-expr color))
      (.beginPath ctx)
      (let [[x y] start-pos]
        (.moveTo ctx x y))
      (doseq [[x y] rest-pos]
        (.lineTo ctx x y))
      (.closePath ctx)
      (.fill ctx))))

(js/setInterval #(render-background settings)
                (:render-interval-ms settings))
;;(render-background)


