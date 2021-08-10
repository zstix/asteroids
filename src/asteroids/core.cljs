(ns asteroids.core
    (:require [reagent.core :as r]
              [reagent.dom :as d]))

(defonce canvas (.getElementById js/document "world"))

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
  (Math/atan2 (- (:y b) (:y a)) (- (:x b) (:x a))))

(defn dist-from-points [a b]
  (Math/sqrt
    (+ (Math/pow (- (:x a) (:x b)) 2)
       (Math/pow (- (:y a) (:y b)) 2))))

(defn rotate-points [center points deg]
  (->> points
       (map (fn [point]
              (point-from-rad
                center
                (+ (rad-from-points center point) (deg-to-rad deg))
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
  (let [points (rotate-points pos (:points e) rotation)]
    (doto ctx
      (set-stroke (or color "white"))
      (.beginPath)
      (draw-circle pos 2) ; for debugging
      (.moveTo (-> points last :x) (-> points last :y))
      (do (run! #(.lineTo ctx (:x %) (:y %)) points))
      (.stroke))))

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

(defn draw-game [{:keys [ctx hero asteroids world] :as state}]
  (do
    (.clearRect ctx 0 0 (:width world) (:height world))
    (draw-entity ctx hero)
    (run! #(draw-entity ctx %) asteroids)
    state))

; TODO: acceleration
(defn update-entity [{:keys [pos] :as entity}]
  (assoc entity :pos {:x (+ (:x pos) (get-in entity [:vel :x] 0))
                      :y (+ (:y pos) (get-in entity [:vel :y] 0))}))

(defn update-entities [{:keys [hero asteroids] :as state}]
  (assoc state
         :hero (update-entity hero)
         :asteroids (map update-entity asteroids)))

; (defn update-game []
;   (do
;     (reset! pos (+ @pos @vel))
;     (if (or (>= @pos limit) (<= @pos 0))
;       (reset! vel (* @vel -1)))))

; (defn loop-game [timestamp]
;   (let [{:keys [last-frame-ms max-fps]} @state]
;     (if
;       (< timestamp (+ last-frame-ms (/ 1000 max-fps)))
;       (.requestAnimationFrame js/window game-loop)
;       (do
;         (reset! state (assoc @state :last-frame-ms timestamp))
;         (update-game)
;         (draw)
;         (.requestAnimationFrame js/window game-loop)))))

;; -------------------------
;; Starting state

(def init-state {:last-frame-ms 0
                 :max-fps 30
                 :ctx (.getContext canvas "2d")
                 :world {:width (.-width canvas)
                         :height (.-height canvas)}
                 :hero {:pos {:x 100 :y 100}
                        :vel {:x 2 :y 2}
                        :color "lime"
                        :rotation 30
                        :points [{:x (+ 0 100) :y (+ -15 100)}
                                 {:x (+ 12 100) :y (+ 15 100)}
                                 {:x (+ -12 100) :y (+ 15 100)}]}
                 :asteroids (map
                              #(generate-asteroid % 200 60)
                              (range 80 700 150))})

;; -------------------------
;; Initialize app

(defn init [{:keys [ctx world hero asteroids] :as state}]
  (do
    (set! (.-lineWidth ctx) 1)
    (draw-game state)))

; (defn mount-root []
;   (d/render [app] (.getElementById js/document "app")))

; (defn ^:export init! []
;   (draw))

(defn ^:dev/after-load start []
  (init init-state))
