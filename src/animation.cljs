(ns animation
  (:require [linalg :as linalg]))

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
   :max-period-time 5
   :tol 1.0e-6})

(defn bounded-random [lower upper]
  (+ lower (* (- upper lower)
              (Math/random))))

(defn rand-minus-plus-1 []
  (bounded-random -1 1))

(defn random-vector-3d []
  (loop []
    (let [x [(rand-minus-plus-1)
             (rand-minus-plus-1)
             (rand-minus-plus-1)]]
      (if (< (linalg/squared-norm x) 1.0)
        x
        (recur)))))

(defn random-unit-vector-3d [vector-generator]
  (loop []
    (let [x (vector-generator)
          y (linalg/normalize-or-nil x)]
      (or y (recur)))))

(defn random-subspace []
  (let [x (random-unit-vector-3d random-vector-3d)
        y (random-unit-vector-3d
           #(linalg/orthogonalize (random-vector-3d) x))]
    [x y]))

(defn initialize-state [{:keys [ref-pts plane-count min-period-time
                                max-period-time tol]}]
  {:pre [(seq ref-pts)
         (number? plane-count)
         (number? min-period-time)
         (number? max-period-time)]}
  {:bounding-box (bounding-box ref-pts)
   :tol tol
   :planes (mapv (fn [plane-index]
                   {:period-time (bounded-random
                                  min-period-time
                                  max-period-time)
                    :offset [0 0 0]
                    :key (str "plane" plane-index)
                    :subspace (random-subspace)})
                 (range plane-count))})

(defn plane-at-time [])

(def two-pi (* 2.0 Math/PI))

(defn polyhedron-from-state [{:keys [planes tol]} time-seconds]
  {:pre [(number? tol)
         (number? time-seconds)]}
  (linalg/polyhedron
   (into {}
         (map (fn [{:keys [period-time offset key subspace]}]
                (let [omega (/ two-pi period-time)
                      phi (* omega time-seconds)
                      cos-phi (Math/cos phi)
                      sin-phi (Math/sin phi)
                      [x-axis y-axis] subspace
                      unit-vec (linalg/add-vectors
                                offset
                                (linalg/add-vectors
                                 (linalg/scale-vector
                                  cos-phi x-axis)
                                 (linalg/scale-vector
                                  sin-phi y-axis)))
                      plane (linalg/plane-with-normal-at-point
                             (linalg/scale-vector -1 unit-vec)
                             unit-vec)]
                  [key plane]))) planes)
   tol))
