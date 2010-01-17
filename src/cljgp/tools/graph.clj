;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.


(ns cljgp.tools.graph
  "Example graphing implementation that plots data about the evolution process
  as it's computed. Requires QN Plot library (http://quies.net/java/math/plot/),
  tested with v1.6.

  Used by mapping the fitness plotter fn over the generated run, ex:
    (last
      (map (create-fitness-plotter) 
           (generate-run my-experiment-config)))

  See also cljgp.tools.logging."
  (:import [javax.swing JFrame JPanel]
           [java.awt Color Graphics Dimension]
           [net.quies.math.plot Graph Function ChartStyle ChartType])
  (:use cljgp.tools.analyse))


(defn- make-style
  "Creates a ChartStyle object of the given Color."
  [#^Color c]
  (doto (new ChartStyle)
    (.setPaint c)))

(defn create-fitness-plotter
  "Creates a QN Plot graph in a JFrame, and returns a function that plots
  fitness statistics for each generation live during the GP run.

  If 'draw-min-only is true, only draws the minimum fitness. This can be
  appropriate when some individuals in the population may have very large
  fitness values, as the graph will scale to fit those, making the other
  statistics difficult to discern."
  ([draw-min-only]
     (let [avg-fit-func (new Function "fitness-average")
           min-fit-func (new Function "fitness-min")
           max-fit-func (new Function "fitness-max")
           graph (doto (new Graph)
                   (.setBackground Color/WHITE)
                   (.addFunction avg-fit-func (make-style 
                                               (new Color 0 0 255 100)))
                   (.addFunction max-fit-func (make-style 
                                               (new Color 0 255 0 100)))
                   (.addFunction min-fit-func (make-style Color/RED)))
           frame (doto (new JFrame "cljgp-plot")
                   (.add graph)
                   (.setSize 800 600)
                   (.validate)
                   (.setVisible true))]
       (fn [generation]
         (when-let [gen-seq (setup-stats-map generation)]
           (let [;; Capture the JFrame in this closure
                 gui frame
                 ;; QN Plot wants BigDecimals for everything
                 gen-num (bigdec (:gen (first gen-seq)))
                 stats (:stats-map (meta gen-seq))
                 {:keys [fit-min fit-max fit-avg]} (get-stat stats
                                                             :fitness-all)]
             (.addPoint min-fit-func gen-num (bigdec fit-min))
             (when (not draw-min-only)
               (.addPoint avg-fit-func gen-num (bigdec fit-avg))
               (.addPoint max-fit-func gen-num (bigdec fit-max)))
             (doto graph
               .render
               .repaint)))
         generation)))
  ([] (create-fitness-plotter false)))
