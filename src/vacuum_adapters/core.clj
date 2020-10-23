(ns vacuum-adapters.core
  (:use [scad-clj.model])
  (:require [scad-clj.scad :as scad]))



(def wall-thickness 2)
(def hose-receptacle-dia 40)
(def hose-receptacle-length 40)
(def lock-size 15)
(def spacing 0.8)


(def hose-receptacle (tube hose-receptacle-dia hose-receptacle-length :wall wall-thickness))
(def hose-receptacle-lock (lock hose-receptacle-dia lock-size wall-thickness))

(def hose-part (union
                hose-receptacle
               (->>
                hose-receptacle-lock
                (translate [0 0 hose-receptacle-length]))
               ))

(def pre-lock-overlap 30)

(def intake-overlap-inner-dia (+ hose-receptacle-dia (* 2 (+ wall-thickness spacing)) ))


(def intake-length 35)
(def intake-to-lock-transition-length 5)
(def intake-far-dia 50)
(def intake-near-dia 50)

(def intake-part (union
                  (->>
                   (tube intake-overlap-inner-dia pre-lock-overlap :wall wall-thickness)
                   (translate [0 0 (- hose-receptacle-length pre-lock-overlap)]))
                  (->>
                   (lock intake-overlap-inner-dia lock-size wall-thickness)
                   (translate [0 0 hose-receptacle-length])
                   )
                  (->>
                   (tube-cone intake-overlap-inner-dia
                              intake-near-dia
                              wall-thickness
                              intake-to-lock-transition-length)
                   (up (+ hose-receptacle-length (* 2 lock-size))))
                  (->>
                   (tube-cone intake-near-dia intake-far-dia wall-thickness intake-length)
                   (up (+ hose-receptacle-length (* 2 lock-size) intake-to-lock-transition-length)))
                  ))

(def thread-profile-dia 4)
(def thread-offset 5)
(def thread-pitch 5.8)
(def thread-rotations 4)
(def thread-shape
  (call-inline "he_rotate" [90 0 0]
               (call-inline "he_translate" [(/ hose-receptacle-dia 2) 0 0]
                            (call-inline "he_circle" "$fn = 20" thread-profile-dia))))

(def thread (up thread-offset (mirror [90 0 0] (call-module "helix_extrude" {:shape thread-shape :pitch thread-pitch :rotations thread-rotations}))))

(def assembly (union
               hose-part
               intake-part
               thread))

(def test-fit (union
               (difference
                hose-receptacle
                (->>
                 (cube (+ (* 3 wall-thickness) hose-receptacle-dia) (+ (* 3 wall-thickness) hose-receptacle-dia) hose-receptacle-length :center true)
                 (up (+ (/ hose-receptacle-length 2) thread-offset thread-profile-dia (* thread-pitch thread-rotations)))))
               thread))

;; hose receptacle + rotating interface + diameter align + connector
;; hose receptacle + diameter align + connector
;; connector 1 + diameter align + connector 2 + inclined long hose receptacle - (insides of connectors and aligner)

;; what I need:
;; - fixed adapter to Metabo
;; - fixed adapter to cyclone
;; - rotating adapter to cyclone
;; (defn fixed-hose-to-metabo []
;;   (let []
;;     (union hose-receptacle-with-thread diameter-align meta))
;;   )




(spit "demo.scad" (scad/write-scad [(use "helix_extrude.scad")
                                    (fn! 128)
                                    assembly
                                    ;; (translate [100 0 0] test-fit)
                                    ]))
