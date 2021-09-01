(ns irm.views
  (:require [lanterna.screen :as s]
            [clojure.string :as st]))


(defn draw-file-map
  "Take a screen, a drawing function and file map, and draws the file map to the screen.
  The drawing function should be a function that takes four arguments: the screen, the name of the file,
  whether or not that file has been selected and the index of the current row."
  [scr draw-fn file-map]
  (doall
   (map-indexed (fn [i [file file-info]] (draw-fn scr file file-info i))
                (sort file-map))))

(defn draw-check-box
  [b]
  (if b
    "[X]"
    "[ ]"))

(defn draw-file [scr file {:keys [selected? depth directory? open?]} i]
  (let [checkbox (draw-check-box selected?)
        dir-symbol (cond
                     (not directory?) " "
                     open? "v"
                     :else ">")
        x-coord (* 2 (or depth 0))]
    (s/put-string scr x-coord (inc i) (st/join " " [dir-symbol checkbox file]))
    (s/redraw scr)))

(defn draw-file-screen [scr file-map path]
  (s/clear scr)
  (s/put-string scr 0 0 (str "Current directory:" path))
  (draw-file-map scr draw-file file-map)
  (s/put-string scr 0 (dec (second (s/get-size scr))) "? for help")
  (s/redraw scr)
  file-map)

(defn draw-help-screen [scr]
  (s/clear scr)
  (doall
    (map-indexed #(s/put-string scr 0 %1 %2)
                 (st/split-lines
                   "up/k/p - up
down/j/n - down
c - select (check)
x - execute
r - refresh
q - quit

Press any button to return.")))
  (s/redraw scr))
