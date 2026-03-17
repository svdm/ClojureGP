;; Copyright (c) Stefan A. van der Meer. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

(ns cljgp.selection
  "Selection functions."
  (:require [cljgp.random :as random]
            [cljgp.tools.analyse :as analyse]))

(defn tournament-select
  "Selects :size individuals from the given population 'pop and returns the one
  with the best (lowest) fitness. May select duplicates into a tournament."
  [{tsize :size, :or {tsize 7}} 
   pop]
  (let [pop-size (count pop)
        competitors (take tsize
                          (repeatedly #(nth pop (random/gp-rand-int pop-size))))]
    (analyse/best-fitness competitors)))
