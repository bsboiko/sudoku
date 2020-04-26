(ns sudoku.db
  (:require [cljs.spec.alpha :as s]))

(s/def ::sudoku-id (s/and int? 
                          #(<= 0 % 8)))
(s/def ::sudoku-num (s/and int?
                           #(<= 1 % 9)))

(s/def :cell/id    (s/and int?
                          #(<= 0 % 80)))
(s/def :cell/row   ::sudoku-id)
(s/def :cell/col   ::sudoku-id)
(s/def :cell/grid  ::sudoku-id)
(s/def :cell/value (s/coll-of ::sudoku-num
                              :kind      set?
                              :distinct  true
                              :min-count 0
                              :max-count 9
                              :into      #{}))

(s/def ::cell (s/keys :req [:cell/id
                            :cell/row
                            :cell/col
                            :cell/grid
                            :cell/value]))

(s/def ::board (s/map-of :cell/id ::cell))

(s/def ::old-board ::board)
(s/def ::new-board (s/nilable ::board))

(s/def ::db (s/keys :req-un [::old-board]
                    :opt-un [::new-board]))

(defn cell->grid
  [{:keys [:cell/row :cell/col]}]
  (cond
    (and (<= 0 col 2)
         (<= 0 row 2)) 0
    (and (<= 3 col 5)
         (<= 0 row 2)) 1
    (and (<= 6 col 8)
         (<= 0 row 2)) 2
    (and (<= 0 col 2)
         (<= 3 row 5)) 3
    (and (<= 3 col 5)
         (<= 3 row 5)) 4
    (and (<= 6 col 8)
         (<= 3 row 5)) 5
    (and (<= 0 col 2)
         (<= 6 row 8)) 6
    (and (<= 3 col 5)
         (<= 6 row 8)) 7
    (and (<= 6 col 8)
         (<= 6 row 8)) 8))

(def data
  (into {}
        (for [[k v]
              {0 1
               1 9
               3 7
               4 5
               5 8
               6 6
               17 7
               19 6
               20 7
               21 9
               23 3
               24 2
               26 1
               29 5
               30 4
               36 9
               37 7
               38 3
               39 2
               45 6
               46 4
               48 8
               52 5
               54 7 
               60 3
               63 3
               67 9
               69 7
               70 4
               71 5
               72 2
               77 6
               80 9}
              :when v]
          [k {:cell/value #{v}}])))

(def default-db
  {:old-board (merge-with merge (into {}
                                      (for [id   (range 81)
                                            :let [row  (quot id 9)
                                                  col  (mod id 9)
                                                  cell {:cell/id    id
                                                        :cell/row   row
                                                        :cell/col   col
                                                        :cell/value #{}}]]
                                        [id (assoc cell
                                                   :cell/grid (cell->grid cell))]))
                          data)})
