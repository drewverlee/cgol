(ns drewverlee.cgol)

;; conways game of life rules
;; 1. Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2. Any live cell with more than three live neighbours dies, as if by overcrowding.
;; 3. Any live cell with two or three live neighbours lives on to the next generation.
;; 4. Any dead cell with exactly three live neighbours becomes a live cell.

;; step one, we translate and reduce these rules into a function which answers the question
;; "should this shell-stay-alive?" we do that by first realizing the rules above can be generalized.

;; First by realizing that the relationship between dead and alive, in this context, is no different then the
;; relationship between the booleans true and false. As in, the cell can either be true or false. only
;; 2 states, the same as Booleans, only 2 states. Compare that to representing the it using the string
;; "Alive" that string has many things you can do to it, such as add more letters, but that functionality
;; is useless, distracting, potential buggy. By picking the right primitives, in this case boolean.
;; we shield ourselves from future error

;; explanation to be continued....

;; here is the code...

(defn- should-this-cell-stay-alive?
  [how-many-neighbors-are-alive? is-the-current-cell-alive?]
  (or
    (= how-many-neighbors-are-alive? 3)
    (and is-the-current-cell-alive? (= how-many-neighbors-are-alive? 2))))

(defn- given-the-current-cell-how-many-neighbors-are-alive-around-it?
  [[row column :as current-cell] grid]
  (->>
    [[(dec row) (dec column)] [(dec row) column] [(dec row) (inc column)]
     [row (dec column)]       [row column]       [row (inc column)]
     [(inc row) (dec column)] [(inc row) column] [(inc row) (inc column)]]
    (remove
      (fn [[row column :as neighbor-cell]]
        (let [max-row-size    (-> grid count)
              max-column-size (-> grid first count)]
          (or
            (= current-cell neighbor-cell)
            ;; remove any cells that go out of bounds of the grid
            (< max-row-size row)
            (< max-column-size column)
            (neg? row)
            (neg? column)))))
    (map #(get-in grid %))
    (filter true?)
    count))

(defn given-a-grid-of-cells-if-we-apply-conways-game-of-life-to-it-what-will-the-resulting-grid-look-like?
  [grid]
  (reduce-kv
    (fn [new-grid row-index row]
      (conj new-grid
            (reduce-kv
              (fn [new-row column-index is-the-current-cell-alive?]
                (conj new-row
                      (should-this-cell-stay-alive?
                        (given-the-current-cell-how-many-neighbors-are-alive-around-it?
                          [row-index column-index] grid)
                        is-the-current-cell-alive?)))
              []
              row)))
    []
    grid))

(=
  (given-a-grid-of-cells-if-we-apply-conways-game-of-life-to-it-what-will-the-resulting-grid-look-like?
    [[false false false false false false false false]
     [false false false false true  false false false]
     [false false false true  true  false false false]
     [false false false false false false false false]])
  [[false false false false false false false false]
   [false false false true  true  false false false]
   [false false false true  true  false false false]
   [false false false false false false false false]])
;; => true
