(ns mrsudoku.solver
  (:use midje.sweet)
  (:require
    [mrsudoku.grid :as g]
    [mrsudoku.engine :as e]))

(def ^:private sudoku-grid (var-get #'g/sudoku-grid))
(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;;recursive function -- ok
(defn solution [grid]
  (loop [res grid, i 1, j 1, num 1, forward true]
    (if (< i 10)
      (if (< j 10)
        (let [cell (g/cell res i j)]
          ;;(println i j cell)
          (if (not= (get cell :status) :init)
            ;;editable, start trying 1 - 9
            (if (<= num 9)
              ;;possible solution
              (let [new-grid (g/change-cell res i j {:status :solved, :value num})]
                ;;(println "new problem" i j (g/cell new-grid i j))
                (if (= (e/grid-conflicts new-grid) {})
                  ;;go to next case
                  (recur new-grid i (inc j) 1 true)
                  ;;try with another number
                  (recur res i j (inc num) true)))
              ;;No solution, clear this case then go to precedent case
              (if (= j 1)
                (recur (g/change-cell res i j {:status :empty}) (dec i) 9 (inc (g/cell-value (g/cell res (dec i) 9))) false)
                (recur (g/change-cell res i j {:status :empty}) i (dec j) (inc (g/cell-value (g/cell res i (dec j)))) false))
              )
            ;;init, go to next/precedent case
            (if forward
              (recur res i (inc j) 1 true)
              (if (= j 1)
                (recur res (dec i) 9 (inc (g/cell-value (g/cell res (dec i) 9))) false)
                (recur res i (dec j) (inc (g/cell-value (g/cell res i (dec j)))) false)))
            ))
        ;;should change to next column
        (recur res (inc i) 1 1 true))
      res)
    )
  )

(fact "Try to have a solution"
      (solution sudoku-grid)
      => [[[{:status :init :value 5}
            {:status :init :value 3}
            {:status :solved :value 4}
            {:status :init :value 6}
            {:status :solved :value 7}
            {:status :solved :value 2}
            {:status :solved :value 1}
            {:status :init :value 9}
            {:status :init :value 8}]
           [{:status :solved :value 6}
            {:status :init :value 7}
            {:status :solved :value 8}
            {:status :init :value 1}
            {:status :init :value 9}
            {:status :init :value 5}
            {:status :solved :value 3}
            {:status :solved :value 4}
            {:status :solved :value 2}]
           [{:status :solved :value 9}
            {:status :solved :value 1}
            {:status :solved :value 2}
            {:status :solved :value 3}
            {:status :solved :value 4}
            {:status :solved :value 8}
            {:status :solved :value 5}
            {:status :init :value 6}
            {:status :solved :value 7}]]
          [[{:status :init :value 8}
            {:status :solved :value 5}
            {:status :solved :value 9}
            {:status :init :value 4}
            {:status :solved :value 2}
            {:status :solved :value 6}
            {:status :init :value 7}
            {:status :solved :value 1}
            {:status :solved :value 3}]
           [{:status :solved :value 7}
            {:status :init :value 6}
            {:status :solved :value 1}
            {:status :init :value 8}
            {:status :solved :value 5}
            {:status :init :value 3}
            {:status :solved :value 9}
            {:status :init :value 2}
            {:status :solved :value 4}]
           [{:status :solved :value 4}
            {:status :solved :value 2}
            {:status :init :value 3}
            {:status :solved :value 7}
            {:status :solved :value 9}
            {:status :init :value 1}
            {:status :solved :value 8}
            {:status :solved :value 5}
            {:status :init :value 6}]]
          [[{:status :solved :value 9}
            {:status :init :value 6}
            {:status :solved :value 1}
            {:status :solved :value 2}
            {:status :solved :value 8}
            {:status :solved :value 7}
            {:status :solved :value 3}
            {:status :solved :value 4}
            {:status :solved :value 5}]
           [{:status :solved :value 5}
            {:status :solved :value 3}
            {:status :solved :value 7}
            {:status :init :value 4}
            {:status :init :value 1}
            {:status :init :value 9}
            {:status :solved :value 2}
            {:status :init :value 8}
            {:status :solved :value 6}]
           [{:status :init :value 2}
            {:status :init :value 8}
            {:status :solved :value 4}
            {:status :solved :value 6}
            {:status :solved :value 3}
            {:status :init :value 5}
            {:status :solved :value 1}
            {:status :init :value 7}
            {:status :init :value 9}]]])

;;recall solution1 but have a bug : the function can't return the correct result in recall
;;try debug with search word "dbg : solution" that you can have the answer in console
(defn solution1 [grid i j]
  (if (and (= i 9) (= (dbg j) 10))
    grid
    (loop [res grid, i i, j j]
      (if (< i 10)
        (if (< j 10)
          (let [cell (g/cell res i j)]
            (println i j cell)
            (if (= (get cell :status) :empty)
              (let [num (atom 1)]
                (while (<= @num 9)
                  (let [new-grid (g/change-cell res i j {:status :solved, :value @num})]
                    (println "new problem" i j (g/cell new-grid i j))
                    (if (= (e/grid-conflicts new-grid) {})
                      (let [solution (solution1 new-grid i (inc j))]
                        (if (not (nil? solution))
                          (do
                            (println "finally, break the while loop")
                            (reset! num 100)
                            (dbg solution))
                          (do
                            (println "there is no answer with the number, inc number" i j @num)
                            (swap! num inc))))
                      (do
                        (println "current case" i j @num)
                        (swap! num inc)))
                    (when (= @num 10)
                      (println "no solution, return nil")
                      res)
                    ))
                )
              (recur res i (+ 1 j))))
          (recur res (inc i) 1))
        res)
      )
    )
  )

