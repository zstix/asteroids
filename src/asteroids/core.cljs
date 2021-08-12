(ns asteroids.core)

;; -------------------------
;; General utils

(defn clog [& args]
  (.log js/console (str args)))

;; -------------------------
;; Math utils

(defn deg-to-rad [angle]
  (* angle (/ Math/PI 180)))

(defn rad-to-deg [angle]
  (/ (* angle 180) Math/PI))

(defn rand-from [a b]
  (+ a (rand-int (- b a))))

(defn point-from-rad [start rad dist]
  {:x (+ (:x start) (* (Math/cos rad) dist))
   :y (+ (:y start) (* (Math/sin rad) dist))})

(defn rad-from-points [a b]
  (Math/atan2
    (- (:y b) (:y a))
    (- (:x b) (:x a))))

(defn dist-from-points [a b]
  (Math/sqrt
    (+ (Math/pow (- (:x a) (:x b)) 2)
       (Math/pow (- (:y a) (:y b)) 2))))

(defn position-points [center points deg]
  (->> points
       (map #(hash-map :x (+ (:x center) (:x %))
                       :y (+ (:y center) (:y %))))
       (map (fn [point]
              (point-from-rad
                center
                (- (rad-from-points center point) (deg-to-rad deg))
                (dist-from-points center point))))))

;; -------------------------
;; Drawing

(defn set-stroke [ctx value]
  (set! (.-strokeStyle ctx) value)
  ctx)

(defn draw-circle [ctx pos radius]
  (doto ctx
    (.beginPath)
    (.arc (:x pos) (:y pos) radius 0 (* 2 Math/PI))
    (.stroke)))

(defn draw-entity [ctx {:keys [pos rotation color] :as e}]
  (let [points (position-points pos (:points e) rotation)]
    (doto ctx
      (set-stroke (or color "white"))
      (.beginPath)
      (draw-circle pos 2) ; for debugging
      (.moveTo (-> points last :x) (-> points last :y))
      (do (run! #(.lineTo ctx (:x %) (:y %)) points))
      (.stroke))))

(defn draw-game [{:keys [ctx hero asteroids world] :as state}]
  (doto ctx
    (.clearRect 0 0 (:width world) (:height world))
    (draw-entity hero)
    (do (run! #(draw-entity ctx %) asteroids)))
  state)

;; -------------------------
;; Game logic

(defn generate-asteroid [x y size]
  {:pos {:x x :y y}
   :rotation 0
   :points (map
             #(point-from-rad
                {:x x :y y}
                (deg-to-rad %)
                (rand-from (- size 5) (+ size 15)))
             (range 0 360 (/ 360 7)))})

; TODO: acceleration
(defn update-entity [{:keys [pos] :as entity}]
  (assoc entity :pos {:x (+ (:x pos) (get-in entity [:vel :x] 0))
                      :y (+ (:y pos) (get-in entity [:vel :y] 0))}))

(defn update-entities [{:keys [hero asteroids] :as state}]
  (assoc state
         :hero (update-entity hero)
         :asteroids (map update-entity asteroids)))

(defn loop-game [{:keys [max-fps last-frame-ms] :as state}]
  (fn [timestamp]
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
                 :asteroids []});(map
                              ; #(generate-asteroid % 200 60)
                              ; (range 80 700 150))})

(defn init [{:keys [ctx world hero asteroids] :as state}]
  (do
    (set! (.-lineWidth ctx) 1)
    (.requestAnimationFrame js/window (loop-game init-state))))

; (defn ^:export init! []
;   (draw))

(defn ^:dev/after-load start []
  (init init-state))
