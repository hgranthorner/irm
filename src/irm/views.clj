(ns irm.views
  (:require [lanterna.screen :as s]
            [clojure.string :as st]
            [irm.utils :as u]))

(defn draw-check-box
  [b]
  (if b
    "[X]"
    "[ ]"))

(defn draw-files
  "Accepts a lanterna screen object, a seq of seqs representing paths and a file map"
  [scr paths file-map]
  (def ^:dynamic *draw-paths* paths)
  (doall
   (map-indexed
    (fn [i path]
      (let [{:keys [selected? directory? open?]} (u/get-in-file-map file-map path)
            checkbox (draw-check-box selected?)
            dir-symbol (cond
                         (not directory?) " "
                         open? "v"
                         :else ">")
            x-coord (* 2 (dec (count path)))]
        (s/put-string scr x-coord (inc i) (st/join " " [dir-symbol checkbox (last path)]))
        (s/redraw scr)))
    paths)))

(defn draw-file-screen [scr file-map paths path]
  (s/clear scr)
  (s/put-string scr 0 0 (str "Current directory:" path))
  (draw-files scr paths file-map)
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
tab - open directory
c - select (check)
x - execute
r - refresh
q - quit

Press any button to return.")))
  (s/redraw scr))
