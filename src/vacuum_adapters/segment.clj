(ns vacuum-adapters.segment
  (:require [vacuum-adapters.shape :as shape]
            [scad-clj.model :as model]
            [scad-clj.scad :as scad]))

(defrecord Config [])
(defrecord State [wall dia])
(defrecord RenderedSegment [height shape extra-parts])

(defn combine-segments [a b]
  (fn [config state]
    (let [[config-a state-a seg-a] (a config state)
          [config-b state-b seg-b] (b config-a state-a)
          seg (->RenderedSegment
               (+ (:height seg-a) (:height seg-b))
               (model/union (:shape seg-a)
                            (shape/up (:height seg-a) (:shape seg-b)))
               (concat (:extra-parts seg-a) (:extra-parts seg-b)))]
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
    [config (assoc state :wall wall) (->RenderedSegment 0 (model/union) [])]))

(defn inner-dia [dia]
  (fn [config state]
    [config (assoc state :dia dia) (->RenderedSegment 0 (model/union) [])]))

(defn outer-dia [dia]
  (fn [config state]
    [config (assoc state :dia (- dia (* 2 (:wall state)))) (->RenderedSegment 0 (model/union) [])]))

(defn tube [height]
  (fn [config state]
    [config state (->RenderedSegment height (shape/tube (:dia state) height :wall (:wall state)) [])]))

(defn hose-thread [& {:keys [pitch rotations offset profile-dia]}]
  (fn [config state]
    (let [profile (->>
                   (model/call-inline "he_circle" "$fn = 20" profile-dia)
                   (model/call-inline "he_translate" [(/ (:dia state) 2) 0 0])
                   (model/call-inline "he_rotate" [90 0 0]))]
      [config state (->RenderedSegment 0 (shape/up offset (model/mirror [90 0 0] (model/call-module "helix_extrude" {:shape profile :pitch pitch :rotations rotations}))) [])])))

(defn rotating-lock [& {:keys [size overlap-length spacing extra-overlap-spacing extra-overlap-seal-gap] :or {size 15 overlap-length 15 spacing 1.0 extra-overlap-spacing 1 extra-overlap-seal-gap 0.2}}]
  (fn [config state]
    (let [inner-dia (:dia state)
          wall (:wall state)
          overlap-dia (+ inner-dia (* 2 spacing) (* 2 wall))
          overlap-dia-with-extra (+ overlap-dia (* 2 extra-overlap-spacing))
          base-overlap-shape (shape/tube overlap-dia-with-extra overlap-length :wall wall)
          overlap-shape (if (> extra-overlap-spacing 0)
                          (model/union
                           (shape/down (+ overlap-length extra-overlap-spacing) base-overlap-shape)
                           (shape/down
                            extra-overlap-spacing
                            (shape/tube-cone overlap-dia-with-extra overlap-dia wall extra-overlap-spacing)))
                          (shape/down overlap-length base-overlap-shape))
          extras (when (> extra-overlap-spacing 0)
                   [(model/difference
                     (shape/tube (+ inner-dia (* 2 wall)) overlap-length :wall (+ spacing extra-overlap-spacing (- extra-overlap-seal-gap)))
                     (shape/down 0.001 (model/cube spacing (* 2 overlap-dia) (* 2 overlap-length) :center false)))])
          inner-lock (shape/lock inner-dia size wall)
          outer-lock (shape/lock overlap-dia size wall)
          shape (model/union overlap-shape inner-lock outer-lock)]
      [config (assoc state :dia overlap-dia) (->RenderedSegment (* 2 size) shape extras)])))

(defn cone [dia length & {:keys [outer]}]
  (fn [config state]
    (let [new-dia (if outer (- dia (* 2 (:wall state))) dia)]
      [config (assoc state :dia new-dia) (->RenderedSegment length (shape/tube-cone (:dia state) new-dia (:wall state) length) [])])))

(defsegment dia-transition [new-dia & {:keys [run-in run-out length outer] :or {run-in 3 run-out 3 length 10}}]
  (tube run-in)
  (cone new-dia length :outer outer)
  (tube run-out))

(defn render-segments [base-name segments & {:keys [fn] :or {fn 128}}]
  (let [[config state rendered] (run-segments segments)]
    (spit (format "%s.scad" base-name)
          (scad/write-scad [(model/use "helix_extrude.scad")
                            (model/fn! fn)
                            (:shape rendered)

                            ]))
    (doseq [[idx shape] (map-indexed vector (:extra-parts rendered))]
      (spit (format "%s-%d.scad" base-name idx)
            (scad/write-scad [(model/use "helix_extrude.scad")
                              (model/fn! fn)
                              shape])))))

(defsegment my-hose-interface []
  (wall-thickness 2)
  (inner-dia 40)
  (hose-thread :profile-dia 4 :pitch 5.8 :rotations 4 :offset 5)
  (tube 30))

(render-segments "demo" [(my-hose-interface)
                              (rotating-lock :size 15 :overlap-length 20 :spacing 1.0 :extra-overlap-spacing 1)
                              (dia-transition 50 :outer true :length 3 :run-in 1 :run-out 0)
                              (tube 35)])



;; (go
;;   (my-hose-interface)
;;   (rotating-lock lock-size)
;;   (outer-dia-transition intake-near-dia)
;;   (outer-cone intake-far-dia))
