(ns animation)

(defn line-km [k m]
  {:k k :m m})

(defn line-mapper [{:keys [k m]}]
  (fn [x] (+ m (* k x))))

(defn evaluate-line [{:keys [k m]} x]
  {:pre [(number? k)
         (number? m)
         (number? x)]}
  (+ (* k x) m))

(defn size? [x]
  (and (vector? x)
       (= 2 (count x))
       (every? number? x)))

(defn bounds [values]
  {:lower (apply min values)
   :upper (apply max values)})

(defn bounding-box [points]
  (mapv bounds (apply map vector points)))

(defn bd-scale [width {:keys [lower upper]}]
  (/ width (- upper lower)))

(defn bd-center [{:keys [lower upper]}]
  (* 0.5 (+ lower upper)))

(defn fit-line-with-scale [x y scale]
  (line-km scale (- y (* scale x))))

(defn fit-camera [[width height :as size]
                  [xbd ybd :as _bbox]]
  (let [scale (min (bd-scale width xbd)
                   (bd-scale height ybd))]
    {:size size
     :xmap (fit-line-with-scale (bd-center xbd) (* 0.5 width) scale)
     :ymap (fit-line-with-scale (bd-center ybd) (* 0.5 height) scale)}))

(defn projector [{:keys [xmap ymap]}]
  (let [xmapper (line-mapper xmap)
        ymapper (line-mapper ymap)]
    (fn [[x y & rest]]
      (into [(xmapper x) (ymapper y)] rest))))

(def default-settings
  {:plane-count 20
   :ref-pts [[0 0 0]
             [1 1 1]]
   :min-period-time 10
   :max-period-time 5})

(defn initialize-state [settings]
  {:bounding-box (bounding-box (:ref-pts settings))
   :planes []})
