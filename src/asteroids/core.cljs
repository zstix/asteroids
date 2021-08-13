(ns asteroids.core)

;; -------------------------
;; General utils

(defn clog [& args]
  (.log js/console (apply str args)))

(defn get-by-id [id] (.getElementById js/document id))

;; -------------------------
;; Math utils

(defn rand-within [a b]
  (+ a (rand-int (- b a))))

(defn deg->rad [x]
  (* x (/ Math/PI 180)))

(defn rad->deg [x]
  (/ (* x 180) Math/PI))

(defn points->rad [a b]
  (Math/atan2
    (- (:y b) (:y a))
    (- (:x b) (:x a))))

(defn points->dist [a b]
  (Math/sqrt
    (+ (Math/pow (- (:x a) (:x b)) 2)
       (Math/pow (- (:y a) (:y b)) 2))))

(defn add-points
  "Adds two points (or point-like maps with :x and :y) together. Returns 0 for
  any keys that don't exist in either map."
  [a b]
  {:x (+ (get a :x 0) (get b :x 0))
   :y (+ (get a :y 0) (get b :y 0))})

(defn calculate-point
  "Given a starting point, an angle (in radians), and a distance (the hypotenuse),
  this will calculate the adjacent point."
  [start rad dist]
  {:x (+ (:x start) (* (Math/cos rad) dist))
   :y (+ (:y start) (* (Math/sin rad) dist))})

(defn position-points
  "Given a center point, a sequence of points (relative to the center point), and
  an angle (in degrees), this will position and rotate the points accordingly."
  [center points deg]
  (->> points
       (map (partial add-points center))
       (map (fn [point]
              (calculate-point
                center
                (- (points->rad center point) (deg->rad deg))
                (points->dist center point))))))

;; -------------------------
;; Drawing

(defn ctx-set! [ctx prop value]
  (case prop
    :line-width (set! (.-lineWidth ctx) value)
    :stroke-style (set! (.-strokeStyle ctx) value)))

(defn draw-circle [ctx pos radius]
  (doto ctx
    .beginPath
    (.arc (:x pos) (:y pos) radius 0 (* 2 Math/PI))
    .stroke))

(defn draw-lines [ctx points]
 (run! #(.lineTo ctx (:x %) (:y %)) points))

(defn draw-entity [ctx {:keys [pos rotation color] :as e}]
  (let [points (position-points pos (:points e) rotation)]
    (doto ctx
      (ctx-set! :stroke-style (or color "white"))
      .beginPath
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

;; TODO: Better sizing for asteroids. (zs 21-08-12)
(defn generate-asteroid [x y size]
  {:pos {:x x :y y}
   :rotation 0
   :points (map
             #(calculate-point
                {:x 0 :y 0}
                (deg->rad %)
                (rand-within (- size 5) (+ size 15)))
             (range 0 360 (/ 360 7)))})

;; TODO: Acceleration. (zs 21-08-12)
(defn update-entity [{:keys [pos vel] :as entity}]
  (assoc entity :pos (add-points pos vel)))

(defn update-entities [{:keys [hero asteroids] :as state}]
  (assoc state
         :hero (update-entity hero)
         :asteroids (map update-entity asteroids)))

(defn tick [f & args]
  (.requestAnimationFrame js/window (apply f args)))

(defn loop-game [{:keys [max-fps last-frame-ms] :as state}]
  (fn [timestamp]
    (if (< timestamp (+ last-frame-ms (/ 1000 max-fps)))
      (tick loop-game state)
      (tick loop-game (-> state
                          (assoc :last-frame-ms timestamp)
                          update-entities
                          draw-game)))))

;; -------------------------
;; Initialize

(defonce canvas (get-by-id "world"))

(def init-state {:last-frame-ms 0
                 :max-fps 30
                 :timestep (/ 1000 60)
                 :ctx (.getContext canvas "2d")
                 :world {:width (.-width canvas)
                         :height (.-height canvas)}
                 :hero {:pos {:x 100 :y 100}
                        :vel {:x 0.8 :y 0.8}
                        :color "lime"
                        :rotation -45
                        :points [{:x 15 :y 0}
                                 {:x -15 :y 12}
                                 {:x -15 :y -12}]}
                 :asteroids (map
                              #(generate-asteroid % 200 60)
                              (range 80 700 150))})

(defn init [state]
  (ctx-set! (:ctx state) :line-width 1)
  (.requestAnimationFrame js/window (loop-game state)))

(defn ^:dev/after-load start [] (init init-state))
