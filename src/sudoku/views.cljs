(ns sudoku.views
  (:require [re-frame.core :refer [subscribe dispatch]]))

(defn cell
  [[k {:keys [:cell/value :cell/id] :as v}]]
  (let [state (cond
                (empty? value)      :blank
                (= 1 (count value)) :solved
                :else               :indeterminate)]
    [:span
     {:class (str "sudoku-cell " (name state))}
     [:div (condp = state
             :solved        (str (first value))
             :blank         ""
             :indeterminate (str value))]]))

(defn grid
  [cells]
  (into [:span.sudoku-grid]
        (for [i cells]
          [cell i])))

(defn board
  [k]
  (let [data @(subscribe [k])]
    (into [:div.sudoku-board]
          (for [[[_ x] [_ y] [_ z]] 
                (partition 3 (group-by (fn [[_ {:keys [:cell/grid]}]]
                                         grid)
                                       data))]
            [:div
             [grid x]
             [grid y]
             [grid z]]))))

(defn app
  []
  [:div
   [board :db/old-board]
   [board :db/new-board]
   [:button.sudoku-next
    {:on-click #(dispatch [:board/next])}
    "Next Step"]])
