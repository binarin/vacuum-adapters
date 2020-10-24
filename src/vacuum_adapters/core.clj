(ns vacuum-adapters.core
  (:use [vacuum-adapters.segment]))

(defsegment my-hose-interface []
  "Starting segment to screw in a vacuum hose that I have around in my shop."
  (wall-thickness 2)
  (inner-dia 40)
  (tube 2)
  (tube-with-thread :profile-dia 4 :pitch 5.8 :rotations 4)
  (tube 5))

(defsegment cyclone-port []
  "Ending segment that goes into my cyclone"
  (dia-transition 50 :outer true)
  (tube 35))

(defsegment metabo-port []
  "End segment for in/out ports on my metabo shopvac"
  (dia-transition 57.9 :outer true)
  (cone 57.6 25 :outer true))

(defsegment tool-connector []
  "End segment for standard 35mm tools connection"
  (dia-transition 35.5 :outer true)
  (cone 34.5 40 :outer true))

(defn -main []
  (render-segments
   "cyclone-adapter-rotating"
   [(my-hose-interface)
    (lock-with-sealing-ring)
    (cyclone-port)])

  (render-segments
   "metabo-adapter-fixed"
   [(my-hose-interface)
    (metabo-port)])

  (render-segments
   "cyclone-adapter-fixed"
   [(my-hose-interface)
    (cyclone-port)])

  (render-segments
   "tool-connector-rotating"
   [(my-hose-interface)
    (lock-with-sealing-ring)
    (tool-connector)
    ]))
