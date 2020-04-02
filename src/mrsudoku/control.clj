
(ns mrsudoku.control
  (:require
   [mrsudoku.grid :as g]
   [mrsudoku.view :as v]
   [mrsudoku.engine :as e]
   [mrsudoku.solver :as s]
   [clojure.pprint :as pp]
   [seesaw.core :refer [alert config! text! invoke-later select]]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn mk-grid-controller
  [grid] (atom {:grid grid}))

(defn change-cell! [ctrl cx cy cell-widget cell]
  (swap! ctrl #(assoc % :grid (g/change-cell (:grid %) cx cy cell)))
  (invoke-later (v/update-cell-view! cell cell-widget))
  (let [ngrid (:grid (deref ctrl))]
    (println (g/grid->str ngrid))))

(defn solve-cell! [ctrl cx cy cell-widget cell]
  (println cx cy cell)
  (swap! ctrl #(assoc % :grid (g/change-cell (:grid %) cx cy cell)))
  (config! cell-widget :text (:value cell))
  (invoke-later (v/update-cell-view! cell cell-widget)))

(defn fetch-cell-widget [ctrl cx cy]
  (let [id-widget (keyword (str "#cell-" cx "-" cy))]
    (if-let [cell-widget (select (:grid-widget (deref ctrl)) [id-widget])]
      cell-widget
      (throw (ex-info "Widget not found" {:cell-x cx :cell-y cy})))))

(defn update-conflicts! [ctrl]
  (let [conflicts (e/grid-conflicts (:grid (deref ctrl)))]
    (println "conflicts = " conflicts)
    (dorun (map (fn [[[cx cy] conflict]]
                  (change-cell! ctrl cx cy (fetch-cell-widget ctrl cx cy) conflict))
                conflicts))))

(defn reset-conflicts! [ctrl]
  (g/do-grid (fn [cx cy cell]
               ;;(println (str "[" cx "," cy "]: " cell))
               (if (= (:status cell) :conflict)
                 (change-cell! ctrl cx cy (fetch-cell-widget ctrl cx cy) {:status :set :value (:value cell)})))
             (:grid (deref ctrl))))

(defn cell-validate-input! [ctrl cell-widget cx cy val]
  ;; first, set the value
  (change-cell! ctrl cx cy cell-widget {:status :set, :value val})
  ;; then, compute the conflicts
  ;; TODO : reset (old) conflicts
  (reset-conflicts! ctrl)
  ;; Then compute the nuew conflicts
  (update-conflicts! ctrl))

(defn cell-clear! [ctrl cell-widget cx cy]
  (change-cell! ctrl cx cy cell-widget {:status :empty})
  (reset-conflicts! ctrl)
  (update-conflicts! ctrl))

(defn cell-input-handler [ctrl cell-widget cell-x cell-y]
  (fn [cell-event]
    (let [doc (.getDocument cell-event)
          input (.getText doc 0 (.getLength doc))]
      ;;(print (str "at cell (" cell-x "," cell-y "): "))
      ;;(println input)
      (if-let [val (and (not= input "")
                        (try (let [val (Integer/parseInt input)]
                               (when (<= 1 val 9)
                                 val))
                             (catch Exception _ nil)))]
        (cell-validate-input! ctrl cell-widget cell-x cell-y val)
        (do
          (cell-clear! ctrl cell-widget cell-x cell-y)
          (invoke-later (text! cell-widget "")))))))

(defn solution-handler [grid ctrl]
  (let [solution (s/solution grid)]
    (println "solution:")
    (println solution)

    (comment (loop [i 1, j 1]
      (if (< i 10)
        (if (< j 10)
          (do
            (solve-cell! ctrl i j (fetch-cell-widget ctrl i j) (g/cell solution i j))
            (recur i (inc j)))
          (recur (inc i) 1))
        ())))

    ;i don't know why it doesn't work
    (g/do-grid (fn [cx cy cell]
                 (when (= (:status cell) :solved)
                   (solve-cell! ctrl cx cy (fetch-cell-widget ctrl cx cy) cell)))
               solution)
    )
  )

