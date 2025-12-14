(ns linalg
  (:require [squint.string :as str]))

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

(defn zero-with-tol? [x tol]
  (<= (Math/abs x) tol))

(defn plane-line-intersection-lambda [plane line tol]
  {:pre [(number? tol)]}
  (let [denom (dot-product (:normal plane) (:direction line))]
    (when-not (zero-with-tol? denom tol)
      (/ (- (:offset plane) (dot-product (:normal plane) (:point line)))
         denom))))

(defn evaluate-line-point [line lambda]
  (add-vectors (scale-vector lambda (:direction line))
               (:point line)))

(defn intersect-planes [plane-a plane-b tol]
  {:pre [(number? tol)]}
  (let [direction (cross-product (:normal plane-a)
                                 (:normal plane-b))]
    (when (< tol (squared-norm direction))
      (let [p (point-on-plane plane-a)
            dir (cross-product (:normal plane-a) direction)
            line-to-intersection (line dir p)
            lambda (plane-line-intersection-lambda
                    plane-b
                    line-to-intersection
                    tol)]
        (line direction (evaluate-line-point line-to-intersection lambda))))))

(defn apply-bound [bounded-line plane-key plane tol]
  (let [line (:line bounded-line)
        d (dot-product (:direction line)
                       (:normal plane))]
    (if (zero-with-tol? d tol)
      bounded-line
      (let [lambda (plane-line-intersection-lambda plane line tol)
            [key-to-update cmp] (if (< d 0.0)
                                  [:upper <]
                                  [:lower >])]
        (update bounded-line
                key-to-update
                (fn [current-bound]
                  (if (or (nil? current-bound)
                          (cmp lambda (:lambda current-bound)))
                    {:lambda lambda :plane plane-key}
                    current-bound)))))))

(defn apply-bounds [line plane-map tol]
  (reduce (fn [dst [plane-key plane]]
            (apply-bound dst plane-key plane tol))
          {:lower nil
           :upper nil
           :line line}
          plane-map))

(defn compose-key [key-vec]
  (str/join "/" (sort key-vec)))

(defn decompose-key [s]
  {:pre [(string? s)]}
  (str/split s #"/"))

(defn bounded-line-corner [bounded-line k]
  (when-let [{:keys [lambda plane]} (get bounded-line k)]
    [(compose-key (conj (:key bounded-line) plane))
     (evaluate-line-point (:line bounded-line) lambda)]))

(defn bounded-line-exists? [bounded-line]
  (let [lower-lambda (-> bounded-line :lower :lambda)
        upper-lambda (-> bounded-line :upper :lambda)]
    (or (nil? lower-lambda)
        (nil? upper-lambda)
        (< lower-lambda upper-lambda))))

(defn neighbor-keys? [a b]
  (let [[a0 a1 :as x] (decompose-key a)
        [b0 b1 :as y] (decompose-key b)]
    (assert (= 2 (count x)))
    (assert (= 2 (count y)))
    (or (= a0 b0)
        (= a0 b1)
        (= a1 b0)
        (= a1 b1))))

(defn order-corner-loop [unordered]
  (when (seq unordered)
    (let [unordered (apply sorted-set unordered)
          k (first unordered)]
      (loop [result [k]
             remaining (disj unordered k)]
        (if (empty? remaining)
          result
          (let [k (peek result)
                k2 (first (filter #(neighbor-keys? k %)
                                  remaining))]
            (recur (conj result k2) (disj remaining k2))))))))

(defn plane-corner-loop [plane-key all-corners]
  (let [unordered (into {}
                        (keep (fn [[k3s pos]]
                                  (let [k2s (->> k3s
                                                 decompose-key
                                                 (remove #(= plane-key %))
                                                 compose-key)]
                                    (when (not= k2s k3s)
                                      [k2s pos]))))
                        all-corners)]
    (mapv (fn [k] {:key (decompose-key k)
                   :position (get unordered k)})
          (order-corner-loop (keys unordered)))))

(defn decorate-plane [plane-key plane all-corners]
  (let [corner-loop (plane-corner-loop plane-key all-corners)]
    (when (seq corner-loop)
      {:key plane-key
       :plane plane
       :corner-loop corner-loop})))

(defn inside-polyhedron? [pos plane-map]
  (not-any? (fn [[_ plane]]
              (neg? (evaluate-plane plane pos)))
            plane-map))

(defn polyhedron [plane-map tol]
  (let [bounded-lines (vec
                       (for [[ak plane-a] plane-map
                             [bk plane-b] plane-map
                             :when (< (compare ak bk) 0)
                             :let [line (intersect-planes
                                         plane-a plane-b tol)]
                             :when line
                             :let [bounded-line (apply-bounds
                                                 line plane-map tol)]
                             :when (bounded-line-exists? bounded-line)]
                         (assoc bounded-line :key [ak bk])))
        corners (into {}
                      (comp (mapcat (fn [bounded-line]
                                      [(bounded-line-corner
                                        bounded-line :lower)
                                       (bounded-line-corner
                                        bounded-line :upper)]))
                            (remove nil?)
                            (filter (fn [[k pos]]
                                      (inside-polyhedron? pos plane-map))))
                      bounded-lines)]
    {:corners corners
     :planes (->> plane-map
                  (into []
                        (keep (fn [[plane-key plane]]
                                (decorate-plane plane-key plane corners))))
                  (sort-by :key))}))
