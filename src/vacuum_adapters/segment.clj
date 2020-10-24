(ns vacuum-adapters.segment
  (:require [vacuum-adapters.shape :as shape]
            [scad-clj.model :as model]
            [scad-clj.scad :as scad]))

(defrecord Config [])
(defrecord State [wall dia])
(defrecord RenderedSegment [height shape extra-parts])

(def empty-rendered-segment (->RenderedSegment 0 nil []))


(defn stack-rendered-segments [a b]
  (let [new-height (+ (:height a) (:height b))
        extra-parts (concat (:extra-parts a) (:extra-parts b))]
    (cond
      (nil? (:shape a)) (->RenderedSegment new-height (shape/up (:height a) (:shape b)) extra-parts)
      (nil? (:shape b)) (->RenderedSegment new-height (:shape a) extra-parts)
      :else (->RenderedSegment new-height (model/union (:shape a) (shape/up (:height a) (:shape b))) extra-parts))))

(defn combine-segments [a b]
  (fn [config state]
    (let [[config-a state-a seg-a] (a config state)
          [config-b state-b seg-b] (b config-a state-a)]
      [config-b state-b (stack-rendered-segments seg-a seg-b)])))

(defn run-segments
  ([segments]
   (run-segments (->Config) (->State 0 0) segments))

  ([config state segments]
   ((reduce combine-segments segments) config state)))

(defmacro defsegment [name args & body]
  (let [has-doc (string? (first body))
        maybe-doctring (if has-doc [(first body)] [])
        body-without-docstring (if has-doc (rest body) body)]
    `(defn ~name [~@args]
       ~@maybe-doctring
       (fn [~'config ~'state]
         (run-segments ~'config ~'state [~@body-without-docstring])))))

(defn wall-thickness [wall]
  "Empty segment that sets wall thickness to a given value."
  (fn [config state]
    [config (assoc state :wall wall) empty-rendered-segment]))

(defn inner-dia [dia]
  "Empty segment that sets inner diameter to a given value."
  (fn [config state]
    [config (assoc state :dia dia) empty-rendered-segment]))

(defn outer-dia [dia]
  "Empty segment that sets outer diameter to a given value.
  This just takes current wall thickness into account."
  (fn [config state]
    [config (assoc state :dia (- dia (* 2 (:wall state)))) empty-rendered-segment]))

(defn tube [length]
  "A tube segment with a given length, using current diameter and wall thickness."
  (fn [config state]
    [config state (->RenderedSegment length (shape/tube (:dia state) length :wall (:wall state)) [])]))

(defn hose-thread-zero-length [& {:keys [pitch rotations offset profile-dia]}]
  (fn [config state]
    (let [profile (String/format nil "he_rotate([90,0,0], he_translate([%f,0,0], he_circle($fn = 20, %f)))" (to-array [(/ (:dia state) 2.0) (double profile-dia)]) )]
      [config state (->RenderedSegment 0 (shape/up offset (model/mirror [90 0 0] (model/call-module "helix_extrude" {:shape profile :pitch pitch :rotations rotations}))) [])])))

(defsegment tube-with-thread [& {:keys [pitch rotations profile-dia]}]
  "A tube segment with internal left thread.
  Length of the tube is just enough to cover a thread with a given pitch and rotations number"
  (hose-thread-zero-length :pitch pitch :rotations rotations :offset (/ profile-dia 2) :profile-dia profile-dia)
  (tube (+ profile-dia (* pitch rotations))))

(defn rotating-lock [& {:keys [size overlap-length spacing extra-overlap-spacing extra-overlap-seal-gap] :or {size 15 overlap-length 15 spacing 1.0 extra-overlap-spacing 1 extra-overlap-seal-gap 0.15}}]
  (fn [config state]
    (let [inner-dia (:dia state)
          wall (:wall state)
          overlap-dia (+ inner-dia (* 2 spacing) (* 2 wall))
          overlap-dia-with-extra (+ overlap-dia (* 2 extra-overlap-spacing))
          base-overlap-shape (shape/tube overlap-dia-with-extra overlap-length :wall wall)
          overlap-shape (if (> extra-overlap-spacing 0)
                          (model/union
                           (shape/up
                            (* 2 extra-overlap-spacing)
                            (shape/down overlap-length base-overlap-shape))
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

(defmacro segment-when [test & body]
  `(fn [~'config ~'state]
     (if ~test
       (run-segments ~'config ~'state [~@body])
       [~'config ~'state ~'empty-rendered-segment])))


(defsegment dia-transition [new-dia & {:keys [run-in run-out length outer] :or {run-in 0 run-out 0}}]
  (segment-when (> run-in 0)
   (tube run-in))
  (cone new-dia (or length (Math/abs (- new-dia (:dia state)))) :outer outer)
  (segment-when (> run-out 0)
   (tube run-out)))

(defn render-segments [base-name segments & {:keys [fn extra-spacing debug-cube-size] :or {fn 128 extra-spacing 100 debug-cube-size 40}}]
  (let [[config state rendered] (run-segments segments)]
    (spit (format "%s.scad" base-name)
          (scad/write-scad [(model/use "helix_extrude.scad")
                            (model/fn! fn)
                            (:shape rendered)
                            ]))
    (spit (format "%s-debug.scad" base-name)
          (let [cut-cube (shape/down 10 (model/cube debug-cube-size debug-cube-size (* 3 (:height rendered)) :center false))]
            (scad/write-scad [(model/use "helix_extrude.scad")
                              (model/fn! fn)
                              (model/difference
                               (:shape rendered)
                               cut-cube)
                              (map-indexed
                               #(model/translate [(* extra-spacing (+ 1 %1)) 0 0] (model/difference %2 cut-cube))
                               (:extra-parts rendered))
                             ])))
    (doseq [[idx shape] (map-indexed vector (:extra-parts rendered))]
      (spit (format "%s-%d.scad" base-name idx)
            (scad/write-scad [(model/use "helix_extrude.scad")
                              (model/fn! fn)
                              shape])))))


(defsegment lock-with-sealing-ring []
  "A reasonable rotating connector with additional seal ring."
  (rotating-lock :size 15 :overlap-length 25 :spacing 1.0 :extra-overlap-spacing 1))

