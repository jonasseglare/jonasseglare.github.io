(ns animation)

(defn line-km [k m]
  {:k k :m m})

(defn evaluate-line [{:keys [k m]} x]
  {:pre [(number? k)
         (number? m)
         (number? x)]}
  (+ (* k x) m))

(defn ortho-camera [xmap ymap]
  {:xmap xmap
   :ymap ymap})

(defn bounds [values]
  [(apply min values)
   (apply max values)])

(defn bounding-box [points]
  (let [vs (mapv bounds (apply map vector points))]
    {:lower (mapv first vs)
     :upper (mapv second vs)}))

