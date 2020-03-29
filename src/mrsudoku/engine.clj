(ns mrsudoku.engine
  (:use midje.sweet)
  (:require [mrsudoku.grid :as g]))

(def ^:private sudoku-grid (var-get #'g/sudoku-grid))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn values
  "Return the set of values of a vector or grid `cells`."
  [cells]
  (into #{} (map (fn [x] (get x :value)) (filter (fn [x] (get x :value)) cells))))

(fact
 (values (g/block sudoku-grid 1)) => #{5 3 6 9 8})

(fact
 (values (g/row sudoku-grid 1)) => #{5 3 7})

(fact
 (values (g/col sudoku-grid 1)) => #{5 6 8 4 7})

(fact
 (values (g/block sudoku-grid 8)) => #{4 1 9 8})

(fact
 (values (g/row sudoku-grid 8)) => #{4 1 9 5})

(fact
 (values (g/col sudoku-grid 8)) => #{6 8 7})

(defn values-except
  "Return the set of values of a vector of cells, except the `except`-th."
  [cells except]
  {:pre [(<= 1 except (count cells))]}
  (loop [res #{}, index 0]
    (if (< index (count cells))
      (if (not= (inc index) except)
        (recur (conj res (get cells index)) (inc index))
        (recur res (inc index)))
      (values res))))

(fact
 (values-except (g/block sudoku-grid 1) 1) => #{3 9 6 8})

(fact
 (values-except (g/block sudoku-grid 1) 4) => #{3 9 5 8})

(defn mk-conflict [kind cx cy value]
  {:status :conflict
   :kind kind
   :value value})

(defn merge-conflict-kind
  [kind1 kind2]
  (cond
    (and (set? kind1) (set? kind2)) (clojure.set/union kind1 kind2)
    (set? kind1) (conj kind1 kind2)
    (set? kind2) (conj kind2 kind1)
    (= kind1 kind2) kind1
    :else (hash-set kind1 kind2)))

(fact
 (merge-conflict-kind :row :row) => :row)

(fact
 (merge-conflict-kind :row :block) => #{:row :block})

(fact
 (merge-conflict-kind :row #{:row :block}) => #{:row, :block})

(fact
 (merge-conflict-kind #{:row :block} :block) => #{:row, :block})

(fact
 (merge-conflict-kind #{:row :block} #{:block :col}) => #{:row :block :col})


(defn merge-conflict [conflict1 conflict2]
  (assoc conflict1 :kind (merge-conflict-kind (:kind conflict1) (:kind conflict2))))

(defn merge-conflicts [& conflicts]
  (apply (partial merge-with merge-conflict) conflicts)) 

(defn update-conflicts
  [conflict-kind cx cy value conflicts]
  (if-let [conflict (get conflicts [cx, cy])]
    (assoc conflicts [cx, cy] (mk-conflict (merge-conflict-kind conflict-kind (:kind conflict))
                                           cx cy value))
    (assoc conflicts [cx, cy] (mk-conflict conflict-kind cx cy value))))

(defn conflict-value [values except cell]
  (when-let [value (g/cell-value cell)]
    (when (and (not= (:status cell) :init)
               (contains? (values-except values except) value))
      value)))

(defn row-conflicts
  "Returns a map of conflicts in a `row`."
  [row cy]
  (let [s (into [] row)]
    (loop [res {}, cx 1]
      (if (<= cx (count s))
        (recur (if-let [cv (conflict-value s cx  (nth s (dec cx)))]
                 (update-conflicts :row cx cy cv res)
                 res)
               (inc cx))
        res))))

(fact
 (row-conflicts (map #(g/mk-cell :set %) [1 2 3 4]) 1) => {})

(fact
 (row-conflicts (map #(g/mk-cell :set %) [1 2 3 1]) 1)
 => {[1 1] {:status :conflict, :kind :row, :value 1},
     [4 1] {:status :conflict, :kind :row, :value 1}})

(fact
 (row-conflicts [{:status :init, :value 8} {:status :empty} {:status :empty} {:status :empty} {:status :init, :value 6} {:status :set, :value 6} {:status :empty} {:status :empty} {:status :init, :value 3}] 4)
 => {[6 4] {:status :conflict, :kind :row, :value 6}})

(defn rows-conflicts [grid]
  (reduce merge-conflicts {}
          (map (fn [r] (row-conflicts (g/row grid r) r)) (range 1 10))))

(defn col-conflicts
  "Returns a map of conflicts in a `col`."
  [col cx]
  (let [s (into [] col)]
    (loop [res {}, cy 1]
      (if (<= cy (count s))
        (recur (if-let [cv (conflict-value s cy  (nth s (dec cy)))]
                 (update-conflicts :col cx cy cv res)
                 res)
               (inc cy))
        res))))

;;; Ecrire les 'fact'  nécessaires...

(fact
  (col-conflicts (map #(g/mk-cell :set %) [1 2 3 1]) 1)
=> {[1 1] {:status :conflict, :kind :col, :value 1},
    [1 4] {:status :conflict, :kind :col, :value 1}})

(defn cols-conflicts
  [grid] (reduce merge-conflicts {}
                 (map (fn [c] (col-conflicts (g/col grid c) c)) (range 1 10))))


(defn block-conflicts
  [block b]
  (g/reduce-block (fn [conflicts index cx cy cell]
                    (if-let [value (conflict-value block index cell)]
                      (update-conflicts :block cx cy value conflicts)
                      conflicts)) {} block b))

;;; Ecrire les 'fact' nécessaires...
(fact
  (block-conflicts [{:status :set, :value 1}
                    {:status :set, :value 2}
                    {:status :set, :value 4}
                    {:status :set, :value 6}
                    {:status :set, :value 7}
                    {:status :set, :value 6}
                    {:status :set, :value 8}
                    {:status :set, :value 8}
                    {:status :set, :value 9}] 1)
  => {[1 2] {:status :conflict, :kind :block, :value 6},
      [3 2] {:status :conflict, :kind :block, :value 6},
      [1 3] {:status :conflict, :kind :block, :value 8},
      [2 3] {:status :conflict, :kind :block, :value 8}})

(defn blocks-conflicts
  [grid]
  (reduce merge-conflicts {}
          (map (fn [b] (block-conflicts (g/block grid b) b)) (range 1 10))))

(defn grid-conflicts
  "Compute all conflicts in the Sudoku grid."
  [grid]
  (merge-conflicts (rows-conflicts grid)
                   (cols-conflicts grid)
                   (blocks-conflicts grid)))

(fact
  (grid-conflicts [[[{:status :init, :value 5}
                     {:status :init, :value 3}
                     {:status :empty}
                     {:status :init, :value 6}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :init, :value 9}
                     {:status :init, :value 8}]
                    [{:status :set, :value 5}
                     {:status :init, :value 7}
                     {:status :empty}
                     {:status :init, :value 1}
                     {:status :init, :value 9}
                     {:status :init, :value 5}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}]
                    [{:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :init, :value 6}
                     {:status :empty}]]
                   [[{:status :init, :value 8}
                     {:status :empty}
                     {:status :empty}
                     {:status :init, :value 4}
                     {:status :empty}
                     {:status :empty}
                     {:status :init, :value 7}
                     {:status :empty}
                     {:status :empty}]
                    [{:status :set, :value 5}
                     {:status :init, :value 6}
                     {:status :empty}
                     {:status :init, :value 8}
                     {:status :empty}
                     {:status :init, :value 3}
                     {:status :empty}
                     {:status :init, :value 2}
                     {:status :empty}]
                    [{:status :empty}
                     {:status :empty}
                     {:status :init, :value 3}
                     {:status :empty}
                     {:status :empty}
                     {:status :init, :value 1}
                     {:status :empty}
                     {:status :empty}
                     {:status :init, :value 6}]]
                   [[{:status :empty}
                     {:status :init, :value 6}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}]
                    [{:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :init, :value 4}
                     {:status :init, :value 1}
                     {:status :init, :value 9}
                     {:status :empty}
                     {:status :init, :value 8}
                     {:status :empty}]
                    [{:status :init, :value 2}
                     {:status :init, :value 8}
                     {:status :empty}
                     {:status :empty}
                     {:status :empty}
                     {:status :init, :value 5}
                     {:status :empty}
                     {:status :init, :value 7}
                     {:status :init, :value 9}]]])
  => {[4 1] {:kind #{:block :col :row} :status :conflict :value 5}
      [4 4] {:kind :col :status :conflict :value 5}})
