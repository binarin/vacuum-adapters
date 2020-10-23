(ns vacuum-adapters.shape
  (:use [scad-clj.model])
  (:require [scad-clj.scad :as scad]))


(def *wall-thickness* 2)

(defn tube
  [dia height & {:keys [wall] :or {wall *wall-thickness*}}]
  (difference
   (cylinder (+ wall (/ dia 2)) height :center false)
   (->>
    (cylinder (/ dia 2) (+ 1 height) :center false)
    (translate [0 0 -0.5]))))

(defn tube-cone
  [dia-1 dia-2 wall height]
  (difference
   (cylinder [(+ wall (/ dia-1 2)) (+ wall (/ dia-2 2))]
             height :center false)
   (->>
    (cylinder [(/ dia-1 2) (/ dia-2 2)] (+ 1 height) :center false)
    (translate [0 0 -0.5]))))

(defn lock
  [dia lock-size wall]
  (union
   (tube-cone dia (+ lock-size dia) wall lock-size)
   (->>
    (tube-cone (+ lock-size dia) dia wall lock-size)
    (translate [0 0 lock-size]))))

(defn up [z & block]
  (apply translate [0 0 z] block))

(defn down [z & block]
  (apply translate [0 0 (- z)] block))
