(ns asteroids.core
    (:require [reagent.core :as r]
              [reagent.dom :as d]))

(defonce canvas (.getElementById js/document "world"))

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

;; -------------------------
;; Drawing

(defn generate-asteroid [x y size]
  {:pos {:x x :y y}
   :points (map
             #(point-from-rad
                {:x x :y y}
                (deg-to-rad %)
                (rand-from (- size 5) (+ size 15)))
             (range 0 360 (/ 360 7)))})

(defn draw-circle [ctx pos radius]
  (doto ctx
    (.beginPath)
    (.arc (:x pos) (:y pos) radius 0 (* 2 Math/PI))
    (.stroke)))

(defn draw-entity [ctx {:keys [pos points color]}]
  (doto ctx
    (do (set! (.-strokeStyle ctx) (or color "white")))
    ; TODO: rotate
    (.beginPath)
    (.moveTo (-> points last :x) (-> points last :y))
    (do (doseq [p points] (.lineTo ctx (:x p) (:y p))))
    (.stroke)))

;; -------------------------
;; Game logic

; (defn draw []
;   (set!
;     (.-style.left (.getElementById js/document "box"))
;     (str @pos "px")))

; (defn update-game []
;   (do
;     (reset! pos (+ @pos @vel))
;     (if (or (>= @pos limit) (<= @pos 0))
;       (reset! vel (* @vel -1)))))

; (defn game-loop [timestamp]
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
                        :points [{:x (+ 0 100) :y (+ -10 100)}
                                 {:x (+ 12 100) :y (+ 20 100)}
                                 {:x (+ -12 100) :y (+ 20 100)}]}
                 :asteroids (map
                              #(generate-asteroid % 200 60)
                              (range 80 700 150))})

;; -------------------------
;; Initialize app

(defn init [{:keys [ctx world hero asteroids]}]
  (doto ctx
    (do (set! (.-strokeStyle ctx) "red"))
    (do (set! (.-lineWidth ctx) 1))
    (.clearRect 0 0 (:width world) (:height world))
    (do (doseq [a asteroids] (draw-entity ctx a)))
    (draw-entity hero)))

; (defn mount-root []
;   (d/render [app] (.getElementById js/document "app")))

; (defn ^:export init! []
;   (draw))

(defn ^:dev/after-load start []
  (init init-state))