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

(comment

  ;; one sudoku row :)
  (let [hints [0 5 0 0 3 0 0 0 0]
        vars (repeatedly 9 lvar)]
    (run 4 [q]
      (== q vars)
      (bind-all vars hints)
      (everyg constrain-to-domain vars)
      (fd/distinct vars)))

  )
;; https://youtu.be/KOzi-YBq3aI?t=4340
