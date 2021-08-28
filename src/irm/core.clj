(ns irm.core
  (:require [lanterna.screen :as s]
            [clojure.string :as st])
  (:import [java.io File])
  (:gen-class))

(defonce *cursor (atom {:x 0 :y 0}))

(defn bounded [small val large]
  (cond
    (<= small val large) val
    (< large val) large
    (< val small) small))

(defn- update-cursor [scr x y]
  (let [[big-x big-y] (s/get-size scr)
        bounded-x #(bounded 0 % big-x)
        bounded-y #(bounded 0 % big-y)]
    (swap! *cursor
           (fn [{x :x y :y} dx dy] {:x (bounded-x (+ x dx)) :y (bounded-y (+ y dy))})
           x
           y)
    (s/move-cursor scr (get @*cursor :x) (get @*cursor :y))
    (s/redraw scr)))

(defn- create-file-map [files]
  (->> files
       (map #(vector % {:selected? false}))
       (into {})))

(defn- draw-file-map
  "Take a screen, a drawing function and file map, and draws the file map to the screen.
  The drawing function should be a function that takes four arguments: the screen, the name of the file,
  whether or not that file has been selected and the index of the current row."
  [scr draw-fn file-map]
  (doall
   (map-indexed (fn [i [file {:keys [selected?]}]] (draw-fn scr file selected? i))
        file-map)))

(defn- check-box
  [b]
  (if b
    "[X]"
    "[ ]"))

(defn draw-file [scr file selected? i]
  (s/put-string scr 0 (inc i) (st/join " " [(check-box selected?) file]))
  (s/redraw scr))

(defn- create-screen [type]
  (let [current-path (.getCanonicalPath (File. "."))
        files (map str (.list (File. current-path)))
        file-map (create-file-map files)
        scr (s/get-screen type)
        draw-fn draw-file]
    (s/start scr)
    (s/put-string scr 0 0 (str "Current directory:" current-path))
    (draw-file-map scr draw-fn file-map)
    (s/redraw scr)
    {:screen scr :file-map file-map}))

(defn- update-file-map
  [scr {:keys [x y]} file-map]
  (let [index (dec y)
        [file {selected :selected?}] (get (vec file-map) index)]
    (draw-file scr file (not selected) index)
    (assoc-in file-map [file :selected?] (not selected))))

(defn- event-loop [scr file-map]
  (loop [file-map file-map]
    (case (s/get-key-blocking scr)
      :down (do (update-cursor scr 0 1) (recur file-map))
      :up (do (update-cursor scr 0 -1) (recur file-map))
      \x (recur (update-file-map scr @*cursor file-map))
      nil)))

(defn -main [& _]
  (let [{scr :screen file-map :file-map} (create-screen :swing)]
    (event-loop scr file-map)
    (s/stop scr)))

(comment
  (-main)
  (let [{scr :screen} (create-screen :swing)]
    (def scr scr))
  (s/start scr)
  (s/stop scr)
  (s/redraw scr)
  (s/clear scr)
  (println (s/get-key-blocking scr))
  ; The beginnings of the event loop
  (loop []
    (case (s/get-key-blocking scr)
      :down (do (update-cursor scr 0 1) (recur))
      :up (do (update-cursor scr 0 -1) (recur))
      nil))
  (map str (.list (File. (.getCanonicalPath (File. ".")))))
  (map-indexed (fn [i f]
                 (s/put-string scr 0 (+ 2 i) f))
   (map str (.list (File. "."))))
  (s/redraw scr)
  (s/stop scr)
  (comment))
