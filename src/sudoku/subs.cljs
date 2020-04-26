(ns sudoku.subs
  (:require [re-frame.core :refer [reg-sub]]))

(reg-sub
 :db/old-board
 (fn [db _]
   (sort-by first <
            (:old-board db))))

(reg-sub
 :db/new-board
 (fn [db _]
   (sort-by first <
            (:new-board db))))
