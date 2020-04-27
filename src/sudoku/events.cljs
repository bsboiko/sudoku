(ns sudoku.events
  (:require [re-frame.core   :refer [reg-event-db after]]
            [cljs.spec.alpha :as s]
            [sudoku.db       :as db :refer [default-db]]
            [sudoku.util     :as u]))

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
  (comp (filter u/solved?)
        (map second)
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

(defn all-opts
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

(defn inspect-col
  [board]
  (into {}
        (for [[id {:keys [:cell/value :cell/col :cell/grid] :as cell}] board]
          [id (if (= 1 (count value))
                cell
                (let [col-adjacent (distinct (map (fn [[_ {grid-n :cell/grid}]]
                                                    grid-n)
                                                  (filter (fn [[_ {col-n   :cell/col
                                                                   grid-n  :cell/grid
                                                                   value-n :cell/value}]]
                                                            (and (not= grid grid-n)
                                                                 (= col col-n)
                                                                 (not= 1 (count value-n))))
                                                          board)))
                      col-inspect  (u/conset
                                    (map (fn [grid-inspect]
                                           (let [grid*     (u/get-grid board grid-inspect)
                                                 col-opts  (u/conset
                                                            (u/get-value
                                                             (filter u/not-solved?
                                                                     (u/get-col grid* col))))
                                                 rest-opts (u/conset
                                                            (u/get-value
                                                             (filter u/not-solved?
                                                                     (u/get-col grid* col true))))]
                                             (clojure.set/difference col-opts rest-opts)))
                                         col-adjacent))]
                  (assoc cell :cell/value (apply disj value col-inspect))))])))

(defn inspect-row
  [board]
  (into {}
        (for [[id {:keys [:cell/value :cell/row :cell/grid] :as cell}] board]
          [id (if (= 1 (count value))
                cell
                (let [row-adjacent (distinct (map (fn [[_ {grid-n :cell/grid}]]
                                                    grid-n)
                                                  (filter (fn [[_ {row-n   :cell/row
                                                                   grid-n  :cell/grid
                                                                   value-n :cell/value}]]
                                                            (and (not= grid grid-n)
                                                                 (= row row-n)
                                                                 (not= 1 (count value-n))))
                                                          board)))
                      row-inspect  (u/conset
                                    (map (fn [grid-inspect]
                                           (let [grid*     (u/get-grid board grid-inspect)
                                                 row-opts  (u/conset
                                                            (u/get-value
                                                             (filter u/not-solved?
                                                                     (u/get-row grid* row))))
                                                 rest-opts (u/conset
                                                            (u/get-value
                                                             (filter u/not-solved?
                                                                     (u/get-row grid* row true))))]
                                             (clojure.set/difference row-opts rest-opts)))
                                         row-adjacent))]
                  (assoc cell :cell/value (apply disj value row-inspect))))])))

(defn inspect-grid
  [board]
  (into {}
        (for [[id {:keys [:cell/value :cell/grid] :as cell}] board]
          [id (if (= 1 (count value))
                cell
                (let [grid-adjacent (u/conset
                                     (u/get-value
                                      (dissoc (into {}
                                                    (filter u/not-solved?
                                                            (u/get-grid board grid)))
                                              id)))
                      diff          (clojure.set/difference value grid-adjacent)]
                  (if (= 1 (count diff))
                    (assoc cell :cell/value diff)
                    cell)))])))

(defn next-board
  [board]
  (-> board
      all-opts
      #_#_#_inspect-col
      inspect-row
      inspect-grid))

(reg-event-db
 :board/next
 interceptors
 (fn [db _]
   (let [board (or (:new-board db)
                   (:old-board db))]
     (if (some (fn [[_ {:keys [:cell/value]}]]
                 (not= 1 (count value)))
               board)
       (let [new-board (next-board board)]
         (cond-> (assoc db :new-board new-board)
           (:new-board db) (assoc :old-board (:new-board db))))
       db))))
