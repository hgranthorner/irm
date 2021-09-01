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

(defn- draw-check-box
  [b]
  (if b
    "[X]"
    "[ ]"))

(defn draw-file [scr file selected? i]
  (s/put-string scr 0 (inc i) (st/join " " [(draw-check-box selected?) file]))
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
    (s/put-string scr 0 (dec (second (s/get-size scr))) "up/k/p - up | down/j/n - down | m - select | x - execute | q - quit")
    (s/redraw scr)
    {:screen scr :file-map file-map}))

(defn- update-file-map
  [scr {:keys [x y]} file-map]
  (let [index (dec y)
        [file {selected :selected?}] (get (vec file-map) index)]
    (draw-file scr file (not selected) index)
    (assoc-in file-map [file :selected?] (not selected))))

(defn- delete-files
  [file-map]
  (->> file-map
       (filter #(-> % second :selected?))))


(defn- handle-input [scr file-map]
  (case (s/get-key-blocking scr)
    (:down \j \n) (do (update-cursor scr 0 1) [file-map true])
    (:up \k \p) (do (update-cursor scr 0 -1) [file-map true])
    \z (do (println "Hello world4") [file-map true])
    \m [(update-file-map scr @*cursor file-map) true]
    \x (do (delete-files file-map) [file-map true])
    \a (do (println "Hi alec") [file-map true])
    nil))

(defn- in-repl? []
  (st/includes? *command-line-args* "middleware"))

(defn- event-loop [scr file-map]
  (loop [file-map file-map
         [new-file-map continue?] (#'handle-input scr file-map)]
    (if continue?
     (recur new-file-map (#'handle-input scr new-file-map)))))

(defn -main [& args]
  (let [{scr :screen file-map :file-map} (create-screen (if (= (first args) "dev") :swing :text))]
    (event-loop scr file-map)
    (s/stop scr)))

(comment
  (.delete (File. "example.txt"))
  (future (-main "dev"))
  (let [{scr :screen} (create-screen :swing)]
    (def scr scr))
  (s/start scr)
  (s/stop scr)
  (s/redraw scr)
  (s/clear scr)
  (println (s/get-key-blocking scr))
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
