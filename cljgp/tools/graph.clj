
;;; cljgp.tools.graph.clj

(ns cljgp.tools.graph
  (:import [javax.swing JFrame JPanel]
	   [java.awt Color Graphics Dimension]
	   [net.quies.math.plot Graph Function ChartStyle ChartType])
  (:use cljgp.tools.analyse))

; Example of how data about the evolution process can be graphed as each
; generation is computed.

; This implementation requires QN Plot library, but any lib capable of realtime
; plotting would work.

(set! *warn-on-reflection* true)

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