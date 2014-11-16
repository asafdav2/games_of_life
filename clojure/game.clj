(require '[clojure.set :as set])

(defn neighbors
  ([x y] 
    (into #{} (for [i [-1 0 1] j [-1 0 1] :when (or (not= i 0) (not= j 0))] (list (+ x i) (+ y j)))))
  ([p] 
    (neighbors (first p) (last p)))
)

(defn neighbors_list [cells] (distinct (mapcat neighbors cells)))

(defn candidates [cells] (distinct (concat cells (neighbors_list cells))))

(defn gameLogic [isCellAlive, neighborsAlive]
  (if isCellAlive
    (if (or (== neighborsAlive 2) (== neighborsAlive 3)) true false)
    (if (== neighborsAlive 3) true false))
)

(defn applyRules [cell, aliveCells]
  (gameLogic (some #{cell} aliveCells) (count (set/intersection aliveCells (neighbors cell))))
)

(defn step [cells] (into #{} (filter (fn [cell] (applyRules cell cells)) (candidates cells))))

(defn steps [cells n] (if (== 0 n) cells (steps (step cells) (- n 1))))

(def initial #{(list 0 0), (list -1 0), (list 1 0), (list 0 -1), (list 0 1)})






