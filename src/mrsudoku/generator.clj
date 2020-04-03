(ns mrsudoku.generator
  (:use midje.sweet)
  (:require
    [mrsudoku.grid :as g]
    [mrsudoku.engine :as e])
  (:import (qqwing QQWing)))

(defn generator []
  (let [gen (new QQWing), sudoku (. gen haveAPuzzle)]
    (loop [res [[;row1
                 [] [] []],
                ;row 2
                [
                 [] [] []],
                ;row 3
                [[] [] []]]
           index 0]
      (if (< index 81)
        (if (zero? (nth sudoku index))
          (recur (g/change-cell res (inc (mod index 9)) (inc (quot index 9)) {:status :empty}) (inc index))
          (recur (g/change-cell res (inc (mod index 9)) (inc (quot index 9)) {:status :init, :value (nth sudoku index)}) (inc index)))
        res)
      )
    ))

(comment (fact "The generated puzzle (but could be very difficult...)"
      ;;  (print (generator))
      => nil))


