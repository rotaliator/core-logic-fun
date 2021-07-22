(ns core-logic-fun.main
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [== defnc membero conso conde run run* lvar everyg succeed]
             :as logic]
            [clojure.core.logic.fd :as fd]))

(defnc is-prod? [a b prod]
  (= (* a b) prod))


(defn find-multiplications
  "Finds pairs of numbers giving desired product"
  [product]
  (run* [a b]
    (fd/in a (fd/interval 1                      (int (Math/sqrt product))))
    (fd/in b (fd/interval (int (Math/sqrt product)) (quot product 2)))
    (is-prod? a b product)))


(comment

  (find-multiplications 2172) ;; => ([2 1086] [3 724] [4 543] [6 362] [12 181])

  (run* [q]
    (conso 1 [2 q 4] [1 2 3 4]))

  (run* [a b]
    (fd/in a (fd/interval 1                      (int (Math/sqrt 2172))))
    (fd/in b (fd/interval (int (Math/sqrt 2172)) (quot 2172 2)))
    (is-prod? a b 2172))





  )
;; => nil
