(ns ^:figwheel-hooks sudoku.core
  (:require [reagent.core  :as r]
            [reagent.dom   :as rdom]
            [re-frame.core :refer [dispatch-sync]]
            [sudoku.subs]
            [sudoku.events]
            [sudoku.views  :as views]))

(enable-console-print!)

(defonce db
  (dispatch-sync [:db/initialize]))

(defn render
  []
  (rdom/render [views/app]
               (js/document.getElementById "app")))

(defn ^:after-load on-reload
  []
  (render))

(render)

