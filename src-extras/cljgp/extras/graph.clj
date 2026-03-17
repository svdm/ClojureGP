;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.


(ns cljgp.extras.graph
  "Example graphing implementation that plots fitness statistics about the
  evolution process as it's computed. Requires XChart (org.knowm.xchart/xchart),
  available via the :graph alias in deps.edn.

  Used by mapping the fitness plotter fn over the generated run, ex:
    (last
      (map (create-fitness-plotter)
           (generate-run my-experiment-config)))

  See also cljgp.tools.logging."
  (:import [org.knowm.xchart XYChartBuilder SwingWrapper])
  (:require [cljgp.tools.analyse :as analyse]))


(defn create-fitness-plotter
  "Returns a function that plots fitness statistics for each generation live
  during the GP run. The chart window is opened on the first generation.

  If draw-min-only is true, only draws the minimum fitness. This can be
  appropriate when some individuals in the population may have very large
  fitness values, as the graph will scale to fit those, making the other
  statistics difficult to discern."
  ([draw-min-only]
   (let [chart (.. (XYChartBuilder.) (title "cljgp fitness") (build))
         _     (.. chart getStyler (setMarkerSize 0))
         gens  (atom [])
         mins  (atom [])
         avgs  (atom [])
         maxs  (atom [])
         sw    (atom nil)]
     (fn [generation]
       (when-let [gen-seq (analyse/setup-stats-map generation)]
         (let [gen-num (double (:gen (first gen-seq)))
               stats   (:stats-map (meta gen-seq))
               {:keys [fit-min fit-max fit-avg]} (analyse/get-stat stats :fitness-all)]
           (swap! gens conj gen-num)
           (swap! mins conj (double fit-min))
           (swap! avgs conj (double fit-avg))
           (swap! maxs conj (double fit-max))
           (if @sw
             (do (.updateXYSeries chart "fit-min" @gens @mins nil)
                 (when-not draw-min-only
                   (.updateXYSeries chart "fit-avg" @gens @avgs nil)
                   (.updateXYSeries chart "fit-max" @gens @maxs nil))
                 (.repaintChart @sw))
             (do (.addSeries chart "fit-min" @gens @mins)
                 (when-not draw-min-only
                   (.addSeries chart "fit-avg" @gens @avgs)
                   (.addSeries chart "fit-max" @gens @maxs))
                 (let [s (SwingWrapper. chart)]
                   (.displayChart s)
                   (reset! sw s))))))
       generation)))
  ([] (create-fitness-plotter false)))
