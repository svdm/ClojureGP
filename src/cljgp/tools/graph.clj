;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;;; cljgp/tools/graph.clj

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

;(set! *warn-on-reflection* true)

(defn- make-style
  [#^Color c]
  (doto (new ChartStyle)
    (.setPaint c)))

(defn create-fitness-plotter
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
	 (when-let [gen-seq (seq generation)]
	   (let [gui frame
		 gen-num (bigdec (:gen (first gen-seq)))
		 avg (fitness-average gen-seq)
		 {:keys [highest lowest]} (fitness-range gen-seq)]
	     (.addPoint min-fit-func gen-num (bigdec lowest))
	     (when (not draw-min-only)
	       (.addPoint avg-fit-func gen-num (bigdec avg))
	       (.addPoint max-fit-func gen-num (bigdec highest)))
	     (doto graph
	       .render
	       .repaint)))
	 generation)))
  ([] (create-fitness-plotter false)))