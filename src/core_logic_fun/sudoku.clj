(ns core-logic-fun.sudoku
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [== defnc membero conso conde
                                        run run* lvar everyg succeed and*]
             :as logic]
            [clojure.core.logic.fd :as fd]))

(defn constrain-to-domain
  "Constrains a logic variable to domain"
  [v]
  (fd/in v (fd/domain 1 2 3 4 5 6 7 8 9)))

(defn bind [var digit]
  (if-not (zero? digit)
    (== var digit)
    succeed))

(defn bind-all [vars hints]
  (and* (map bind vars hints)))

(defn rows [elements]
  (mapv vec (partition 9 elements)))

(defn transpose [matrix]
  (into [] (apply map vector matrix)))

(defn cells [elements]
  (let [top-lefts   [0 3 6 27 30 33 54 57 60]
        shifts      [0 1 2 9 10 11 18 19 20]
        cells-idxes (for [top-left top-lefts]
                      (mapv #(+ % top-left) shifts))]
    (mapv (fn [cell-idxes] (mapv #(nth elements %)  cell-idxes)) cells-idxes)))

(defn solve-sudoku [hints]
  (let [vars      (repeatedly (count hints) lvar)
        rows'     (rows vars)
        columns   (transpose rows')
        cells'    (cells vars)
        solutions (run 1 [q]
                    (== q vars)
                    (bind-all vars hints)
                    (everyg constrain-to-domain vars)
                    (everyg fd/distinct rows')
                    (everyg fd/distinct columns)
                    (everyg fd/distinct cells'))]
    (->> solutions
         (mapv vec)
         (mapv rows))))

(comment

  ;; one sudoku row :)
  (let [hints [0 5 0 0 3 0 0 0 0]
        vars  (repeatedly 9 lvar)]
    (run 4 [q]
      (== q vars)
      (bind-all vars hints)
      (everyg constrain-to-domain vars)
      (fd/distinct vars)))

  ;; multiple rows
  (let [hints [0 5 0 0 3 0 0 0 0
               7 0 0 0 0 9 0 0 0]
        vars  (repeatedly (count hints) lvar)
        rows  (rows vars)]
    (run 3 [q]
      (== q vars)
      (bind-all vars hints)
      (everyg constrain-to-domain vars)
      (everyg fd/distinct rows)))

  ;; constrain on columns
  (let [hints   [0 5 0 0 3 0 0 0 0
                 7 0 0 0 0 9 0 0 0]
        vars    (repeatedly (count hints) lvar)
        rows    (rows vars)
        columns (transpose rows)]
    (run 3 [q]
      (== q vars)
      (bind-all vars hints)
      (everyg constrain-to-domain vars)
      (everyg fd/distinct rows)
      (everyg fd/distinct columns)))

  (solve-sudoku [6 0 0  0 0 0  0 4 0
                 1 8 0  0 0 3  5 0 0
                 3 0 7  0 0 0  6 8 0

                 7 0 8  0 0 0  0 6 0
                 0 0 3  4 0 0  0 0 0
                 0 0 0  0 5 2  0 3 0

                 0 0 0  0 4 1  0 7 9
                 0 0 0  3 2 0  0 0 0
                 0 0 0  0 0 0  2 0 0])

;; => [[[6 9 2 5 1 8 7 4 3]
;;      [1 8 4 6 7 3 5 9 2]
;;      [3 5 7 2 9 4 6 8 1]
;;      [7 2 8 1 3 9 4 6 5]
;;      [5 1 3 4 8 6 9 2 7]
;;      [9 4 6 7 5 2 1 3 8]
;;      [2 6 5 8 4 1 3 7 9]
;;      [4 7 9 3 2 5 8 1 6]
;;      [8 3 1 9 6 7 2 5 4]]]

  )
