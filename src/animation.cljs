(ns animation)

(defn dot-product [a b]
  (loop [sum 0.0
         i 0]
    (if (= i (count a))
      sum
      (recur (+ sum (* (nth a i) (nth b i))) (inc i)))))

(defn add-vectors [a b]
  (mapv (fn [a b] (+ a b)) a b))

(defn plane [normal offset]
  {:normal normal
   :offset offset})

(defn line [direction point]
  {:direction direction
   :point point})

(defn plane-with-normal-at-point [normal point]
  (plane normal (dot-product normal point)))

(defn scale-vector [lambda X]
  (mapv #(* lambda %) X))

(defn squared-norm [x]
  (dot-product x x))

(defn point-on-plane [plane]
  (let [lambda (/ (:offset plane)
                  (squared-norm (:normal plane)))]
    (scale-vector lambda (:normal plane))))

(defn evaluate-plane [{:keys [normal offset]} point]
  (- (dot-product normal point)
     offset))

(def default-settings {:tol 1.0e-6})

(defn- cross-product-element [a b c d]
  (- (* a b) (* c d)))

(defn cross-product [[a b c] [x y z]]
  [(cross-product-element b z c y)
   (cross-product-element c x a z)
   (cross-product-element a y b x)])

(defn plane-line-intersection-lambda [plane line]
  (/ (- (:offset plane) (dot-product (:normal plane) (:point line)))
     (dot-product (:normal plane) (:direction line))))

(defn evaluate-line-point [line lambda]
  (add-vectors (scale-vector lambda (:direction line))
               (:point line)))

(defn intersect-planes [plane-a plane-b {:keys [tol]}]
  {:pre [(number? tol)]}
  (let [direction (cross-product (:normal plane-a)
                                 (:normal plane-b))]
    (when (< tol (squared-norm direction))
      (let [p (point-on-plane plane-a)
            dir (cross-product (:normal plane-a) direction)
            line-to-intersection (line dir p)
            lambda (plane-line-intersection-lambda
                    plane-b
                    line-to-intersection)]
        (line direction (evaluate-line-point line-to-intersection lambda))))))

#_(defn polyhedron [planes]
    )
