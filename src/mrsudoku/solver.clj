(ns mrsudoku.solver
  (:use midje.sweet)
  (:require
    [mrsudoku.grid :as g]
    [mrsudoku.engine :as e]))

(def ^:private sudoku-grid (var-get #'g/sudoku-grid))
(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn eliminate
  ;; return the set of answers after eliminated conflict numbers
  ;#{1,2,...}
  [grid cx cy]
  (if (and (< cx 10) (< cy 10))
    (if (= (get (g/cell grid cx cy) :status) :empty)
      (let [block-x (quot (- cx 1) 3),
            block-y (quot (- cy 1) 3),
            bv (e/values ((grid block-y) block-x))
            rv (e/values (g/row grid cy))
            cv (e/values (g/col grid cx))]
        (clojure.set/difference #{1,2,3,4,5,6,7,8,9} bv rv cv))
      nil)
    nil)
  )

(comment (fact "test for eliminating one case"
      (eliminate sudoku-grid 1 3) => #{1 2}))


;;recursive function -- ok
(defn solution1 [grid]
  (loop [res (g/reduce-grid eliminate grid grid), i 1, j 1, forward true, reset (g/reduce-grid eliminate grid grid)]
    (if (< i 10)
      (if (< j 10)
        (let [cell (g/cell res i j)]
          ;(println i j cell)
          (if (not= (get cell :status) :init)
            ;;editable, start trying from possible answers
            (let [ans (get cell :ans)]
              (if (seq ans)
                ;;solution exists
                (let [new-grid (g/change-cell res i j {:status :solved, :value (first ans), :ans (rest ans)})]
                  (if (= (e/grid-conflicts new-grid) {})
                    ;;go to next case
                    (recur new-grid i (inc j) true reset)
                    ;;try with another number
                    (recur new-grid i j true reset)))
                ;;No solution, clear this case then go back to precedent case
                (if (= j 1)
                  (recur (g/change-cell res i j (g/cell reset i j)) (dec i) 9  false reset)
                  (recur (g/change-cell res i j (g/cell reset i j)) i (dec j)  false reset))
                ))
            ;;init, go to next/precedent case
            (if forward
              (recur res i (inc j) true reset)
              (if (= j 1)
                (recur res (dec i) 9  false reset)
                (recur res i (dec j)  false reset)))
            ))
        ;;should change to next column
        (recur res (inc i) 1 true reset))
      res)
    )
  )

(defn solution
  [grid]
  (loop [cx 1, cy 1, grid grid, ans (eliminate grid cx cy)]
    (if (< cx 10)
      (if (< cy 10)
        (let [cell (g/cell grid cx cy)]
          (if (= (get cell :status) :empty)
            (if (seq ans)
              (let [new-grid (g/change-cell grid cx cy {:status :solved, :value (first ans)})]
                (let [vec (solution new-grid)]
                  (if (first vec)
                    [true (second vec)]
                    (recur cx cy grid (rest ans))))
                )
              [false grid])
            (recur cx (inc cy) grid (eliminate grid cx (inc cy)))))
        (recur (inc cx) 1 grid (eliminate grid (inc cx) 1)))
      [true grid])))

(fact "Try to have a solution"
      (second (solution sudoku-grid))
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

(fact "execution time"
      (time (solution sudoku-grid)))

