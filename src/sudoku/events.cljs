(ns sudoku.events
  (:require [re-frame.core   :refer [reg-event-db after]]
            [cljs.spec.alpha :as s]
            [sudoku.db       :as db :refer [default-db]]))

(defn check-and-throw
  [a-spec db]
  (when-not (s/valid? a-spec db)
    (throw (ex-info (str "Spec check failed: " (s/explain-str a-spec db))
                    {}))))

(def check-spec-interceptor
  (after (partial check-and-throw ::db/db)))

(def interceptors
  [check-spec-interceptor])

(reg-event-db
 :db/initialize
 interceptors
 (fn [_ _]
   default-db))

(def nums
  (into #{}
        (range 1 10)))

(def xf-opts
  (comp (map second)
        (filter (fn [{:keys [:cell/value]}]
                  (= 1 (count value))))
        (map :cell/value)))

(def opts (partial transduce xf-opts concat))

(defn cell->opts
  [board id k]
  (let [cell (get board id)
        x    (get cell k)]
    (opts
     (filter (fn [[_ v]]
               (= (get v k) x))
             board))))

(defn step
  [board]
  (into {}
        (for [[id {:keys [:cell/value] :as cell}] board]
          [id (if (= 1 (count value))
                cell
                (let [row  (cell->opts board id :cell/row)
                      col  (cell->opts board id :cell/col)
                      grid (cell->opts board id :cell/grid)
                      all  (apply disj nums
                                  (into #{} (concat row col grid)))]
                  (assoc cell :cell/value all)))])))

(reg-event-db
 :board/next
 interceptors
 (fn [db _]
   (if (:new-board db)
     (let [new-board (step (:new-board db))]
       (assoc db
              :new-board new-board
              :old-board (:new-board db)))
     (let [new-board (step (:old-board db))]
       (assoc db :new-board new-board)))))
