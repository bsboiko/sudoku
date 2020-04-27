(ns sudoku.util)

(defn solved?
  [[_ {value :cell/value}]]
  (= 1 (count value)))

(def not-solved?
  (complement solved?))

(defn conset
  [coll]
  (->> coll
       (apply concat)
       (into #{})))

(defn get-grid
  [board grid]
  (filter (fn [[_ {grid-n :cell/grid}]]
            (= grid grid-n))
          board))

(defn get-col
  [board col & [not?]]
  (filter (fn [[_ {col-n :cell/col}]]
            ((if not?
               not=
               =)
             col col-n))
          board))

(defn get-row
  [board row & [not?]]
  (filter (fn [[_ {row-n :cell/row}]]
            ((if not?
               not=
               =)
             row row-n))
          board))

(defn get-value
  [coll]
  (map (fn [[_ {value :cell/value}]]
         value)
       coll))

(defn check*
  [coll]
  (let [values (concat
                (get-value
                 (filter solved? coll)))]
    (if (seq values)
      (apply distinct? values)
      true)))

(defn check-col
  [board col]
  (check* (get-col board col)))

(defn check-row
  [board row]
  (check* (get-row board row)))

(defn check-grid
  [board grid]
  (check* (get-grid board grid)))

(defn check-board
  [board]
  (let [cols  (every? (partial check-col board) (range 9))
        rows  (every? (partial check-row board) (range 9))
        grids (every? (partial check-grid board) (range 9))]
    (and cols rows grids)))
