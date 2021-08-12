(ns asteroids.core)

;; -------------------------
;; General utils

(defn clog [& args]
  (.log js/console (str args)))

;; -------------------------
;; Math utils

(defn rand-within [a b]
  (+ a (rand-int (- b a))))

(defn deg->rad [angle]
  (* angle (/ Math/PI 180)))

(defn rad->deg [angle]
  (/ (* angle 180) Math/PI))

(defn calculate-point
  "Given a starting point, an angle (in radians), and a distance (the hypotenuse),
  this will calculate the adjacent point."
  [start rad dist]
  {:x (+ (:x start) (* (Math/cos rad) dist))
   :y (+ (:y start) (* (Math/sin rad) dist))})

(defn points->rad [a b]
  (Math/atan2
    (- (:y b) (:y a))
    (- (:x b) (:x a))))

(defn points->dist [a b]
  (Math/sqrt
    (+ (Math/pow (- (:x a) (:x b)) 2)
       (Math/pow (- (:y a) (:y b)) 2))))

(defn position-points
  "Given a center point, a sequence of points (relative to the center point), and
  an angle (in degrees), this will position and rotate the points accordingly."
  [center points deg]
  ;; FIXME: This is adding extra distance from the center. (zs 21-08-12)
  (->> points
       (map #(hash-map :x (+ (:x center) (:x %))
                       :y (+ (:y center) (:y %))))
       (map (fn [point]
              (calculate-point
                center
                (- (points->rad center point) (deg->rad deg))
                (points->dist center point))))))

;; -------------------------
;; Drawing

(defmulti ctx-set! (fn [_ctx prop _value] prop))

(defmethod ctx-set! :stroke-style [ctx _prop value]
  (set! (.-strokeStyle ctx) value))

(defmethod ctx-set! :line-width [ctx _prop value]
  (set! (.-lineWidth ctx) value))

(defn draw-circle [ctx pos radius]
  (doto ctx
    (.beginPath)
    (.arc (:x pos) (:y pos) radius 0 (* 2 Math/PI))
    (.stroke)))

(defn draw-lines [ctx points]
  (run! #(.lineTo ctx (:x %) (:y %)) points))

(defn draw-entity [ctx {:keys [pos rotation color] :as e}]
  (let [points (position-points pos (:points e) rotation)]
    (doto ctx
      (ctx-set! :stroke-style (or color "white"))
      .beginPath
      ;; FIXME: Remove after debugging is complete. (zs 21-08-12)
      (draw-circle pos 2)
      (.moveTo (-> points last :x) (-> points last :y))
      (draw-lines points)
      .stroke)))

(defn draw-entities [ctx entities]
  (run! (partial draw-entity ctx) entities))

(defn draw-game [{:keys [ctx hero asteroids world] :as state}]
  (doto ctx
    (.clearRect 0 0 (:width world) (:height world))
    (draw-entity hero)
    (draw-entities asteroids))
  state)

;; -------------------------
;; Game logic

(defn generate-asteroid [x y size]
  {:pos {:x x :y y}
   :rotation 0
   :points (map
             #(calculate-point
                {:x x :y y}
                (deg->rad %)
                (rand-within (- size 5) (+ size 15)))
             (range 0 360 (/ 360 7)))})

(defn update-entity [{:keys [pos] :as entity}]
  ;; TODO: Acceleration. (zs 21-08-12)
  (assoc entity :pos {:x (+ (:x pos) (get-in entity [:vel :x] 0))
                      :y (+ (:y pos) (get-in entity [:vel :y] 0))}))

(defn update-entities [{:keys [hero asteroids] :as state}]
  (assoc state
         :hero (update-entity hero)
         :asteroids (map update-entity asteroids)))

(defn loop-game [{:keys [max-fps last-frame-ms] :as state}]
  (fn [timestamp]
    ;; HACK: This reads a little messy - consider refactor. (zs 21-08-12)
    (.requestAnimationFrame
      js/window
      (loop-game
        (if
          (< timestamp (+ last-frame-ms (/ 1000 max-fps)))
          state
          (-> state
              (assoc :last-frame-ms timestamp)
              update-entities
              draw-game))))))

;; -------------------------
;; Initialize

(defonce canvas (.getElementById js/document "world"))

(def init-state {:last-frame-ms 0
                 :max-fps 30
                 :timestep (/ 1000 60)
                 :ctx (.getContext canvas "2d")
                 :world {:width (.-width canvas)
                         :height (.-height canvas)}
                 :hero {:pos {:x 100 :y 100}
                        :vel {:x 0.08 :y 0.08}
                        :color "lime"
                        :rotation 30
                        :points [{:x (+ 15 100) :y (+ 0 100)}
                                 {:x (+ -15 100) :y (+ 12 100)}
                                 {:x (+ -15 100) :y (+ -12 100)}]}
                 :asteroids (map
                              #(generate-asteroid % 200 60)
                              (range 80 700 150))})

(defn init [{:keys [ctx world hero asteroids] :as state}]
  (ctx-set! ctx :line-width 1)
  (.requestAnimationFrame js/window (loop-game init-state)))

; (defn ^:export init! []
;   (draw))

(defn ^:dev/after-load start []
  (init init-state))
