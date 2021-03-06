(ns vacuum-adapters.shape
  (:use [scad-clj.model :exclude [import use]])
  (:require [scad-clj.scad :as scad]))

(defn tube
  [dia height & {:keys [wall]}]
  (difference
   (cylinder (+ wall (/ dia 2)) height :center false)
   (->>
    (cylinder (/ dia 2) (+ 1 height) :center false)
    (translate [0 0 -0.5]))))

(defn tube-cone
  [dia-1 dia-2 wall height]
  (difference
   (cylinder [(+ wall (/ dia-1 2.0)) (+ wall (/ dia-2 2.0))]
             height :center false)
   (->>
    (cylinder [(/ dia-1 2.0) (/ dia-2 2.0)] (+ 0.001 height) :center false)
    (translate [0 0 -0.0005]))))

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
