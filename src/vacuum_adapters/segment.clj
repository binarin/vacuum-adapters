(ns vacuum-adapters.segment
  (:require [vacuum-adapters.shape :as shape]
            [scad-clj.model :as model]
            [scad-clj.scad :as scad]))

(defrecord Config [])
(defrecord State [wall dia])
(defrecord SizedShape [height shape])

(defn combine-segments [a b]
  (fn [config state]
    (let [[config-a state-a seg-a] (a config state)
          [config-b state-b seg-b] (b config-a state-a)
          seg (->SizedShape (+ (:height seg-a) (:height seg-b))
                         (model/union (:shape seg-a)
                                      (shape/up (:height seg-a) (:shape seg-b))))]
      [config-b state-b seg])))

(defn run-segments
  ([segments]
   (run-segments (->Config) (->State 0 0) segments))

  ([config state segments]
   ((reduce combine-segments segments) config state)))

(defmacro defsegment [name args & body]
  `(defn ~name [~@args]
     (fn [config# state#]
       (run-segments config# state# [~@body]))))

(defn wall-thickness [wall]
  (fn [config state]
    [config (assoc state :wall wall) (->SizedShape 0 (model/union))]))

(defn inner-dia [dia]
  (fn [config state]
    [config (assoc state :dia dia) (->SizedShape 0 (model/union))]))

(defn tube [height]
  (fn [config state]
    [config state (->SizedShape height (shape/tube (:dia state) height :wall (:wall state)))]))

(defn hose-thread [& {:keys [pitch rotations offset profile-dia]}]
  (fn [config state]
    (let [profile (->>
                   (model/call-inline "he_circle" "$fn = 20" profile-dia)
                   (model/call-inline "he_translate" [(/ (:dia state) 2) 0 0])
                   (model/call-inline "he_rotate" [90 0 0]))]
      [config state (->SizedShape 0 (shape/up offset (model/mirror [90 0 0] (model/call-module "helix_extrude" {:shape profile :pitch pitch :rotations rotations}))))])))

(defsegment my-hose-interface []
  (wall-thickness 2)
  (inner-dia 40)
  (hose-thread :profile-dia 4 :pitch 5.8 :rotations 4 :offset 5)
  (tube 40))

(defn rotating-lock [& {:keys [size overlap-length spacing]}]
  (fn [config state]
    (let [inner-dia (:dia state)
          wall (:wall state)
          overlap-dia (+ inner-dia (* 2 spacing) (* 2 wall))
          overlap-shape (shape/down overlap-length (shape/tube overlap-dia overlap-length :wall wall))
          inner-lock (shape/lock inner-dia size wall)
          outer-lock (shape/lock overlap-dia size wall)
          shape (model/union overlap-shape inner-lock outer-lock)]
      [config (assoc state :dia overlap-dia) (->SizedShape (* 2 size) shape)])))

(defn cone [dia length & {:keys [outer]}]
  (fn [config state]
    (let [new-dia (if outer (- dia (:wall state)) dia)]
      [config (assoc state :dia new-dia) (->SizedShape length (shape/tube-cone (:dia state) new-dia (:wall state) length))])))

(defsegment dia-transition [new-dia & {:keys [run-in run-out length outer] :or {run-in 3 run-out 3 length 10}}]
  (tube run-in)
  (cone new-dia length :outer outer)
  (tube run-out))

(defn render-segments [file segments & {:keys [fn] :or {fn 128}}]
  (let [[config state sized-shape] (run-segments segments)]
    (spit file (scad/write-scad [(model/use "helix_extrude.scad")
                                 (model/fn! fn)
                                 (:shape sized-shape)]))))

(render-segments "demo.scad" [(my-hose-interface)
                              (rotating-lock :size 15 :overlap-length 30 :spacing 1.0)
                              (dia-transition 50 :outer true)
                              (tube 35)])

;; (go
;;   (my-hose-interface)
;;   (rotating-lock lock-size)
;;   (outer-dia-transition intake-near-dia)
;;   (outer-cone intake-far-dia))
