(ns irm.core
  (:require [irm.views :as v]
            [irm.utils :as u]
            [lanterna.screen :as s])
  (:import [java.io File])
  (:gen-class))

(defonce *cursor (atom {:x 0 :y 0}))

(defn- current-dir []
  (.getCanonicalPath (File. ".")))

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

(defn- create-file-map
  [dir-str]
  (let [file-names (map str (.list (File. dir-str)))
        files (map #(File. %) file-names)]
    (->> files
         (map #(vector (str (.getName %))
                       {:selected? false
                        :directory? (boolean (.isDirectory %))
                        :open? false}))
         (into (sorted-map)))))

(defn- create-screen [type]
  (let [file-map (create-file-map (current-dir))
        scr (s/get-screen type)]
    (s/start scr)
    (def ^:dynamic *fm* file-map)
    (v/draw-file-screen scr file-map (u/file-map->paths file-map) (current-dir))
    {:screen scr :file-map file-map}))

(defn- get-file-by-index [file-map index]
  (get (vec file-map) index))

(defn- select-file
  [{:keys [y]} file-map]
  (let [index (dec y)
        [file {selected :selected?}] (get-file-by-index file-map index)]
    (assoc-in file-map [file :selected?] (not selected))))

(defn- toggle-directory
  [{:keys [y]} file-map]
  (let [index (dec y)
        [file {open :open?}] (get-file-by-index file-map index)]
    (-> (assoc-in file-map [file :open?] (not open))
        (assoc-in [file :children] (create-file-map file)))))

(defn- delete-files
  [file-map]
  (doall (->> file-map
              (filter #(-> % second :selected?))
              (map #(.delete (File. (first %)))))))

(defn- handle-input [scr file-map]
  (def ^:dynamic *fm* file-map)
  (let [draw-fn (fn [fm]
                  (print (u/file-map->paths fm))
                  (v/draw-file-screen scr fm (u/file-map->paths fm) (current-dir)))]
    (case (s/get-key-blocking scr)
      (:down \j \n) (do
                      (update-cursor scr 0 1)
                      [file-map true])
      (:up \k \p) (do
                    (update-cursor scr 0 -1)
                    [file-map true])
      \c (let [new-file-map (select-file @*cursor file-map)]
           (draw-fn new-file-map)
           [new-file-map true])
      \x (do
           (delete-files file-map)
           [(draw-fn (create-file-map (current-dir))) true])
      \q [file-map false]
      \r [(draw-fn (create-file-map (current-dir))) true]
      \? (do
           (v/draw-help-screen scr)
           (s/get-key-blocking scr)
           (draw-fn file-map)
           [file-map true])
      :tab (let [new-file-map (toggle-directory @*cursor file-map)]
             (draw-fn new-file-map)
             [new-file-map true])
      [file-map true])))

(defn- event-loop [scr file-map]
  (loop [file-map file-map
         [new-file-map continue?] (#'handle-input scr file-map)]
    (if continue?
      (recur new-file-map (#'handle-input scr new-file-map))
      nil)))

(defn -main [& args]
  (let [{scr :screen file-map :file-map} (create-screen (if (= (first args) "dev") :swing :text))]
    (event-loop scr file-map)
    (s/stop scr)))

(comment
  (get-in *fm* (interpose :children (nth)
                             (u/file-map->paths *fm*)
                             6))
  (.delete (File. "example.txt"))
  (def ^:dynamic *term* (future (-main "dev")))
  (future-cancel *term*)
  (future-cancelled? *term*)
  (let [paths (u/file-map->paths *fm*)
        path (first paths)]
    (get-in *fm* (if (seq? path)
                   path
                   [path])))
  *fm*
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
