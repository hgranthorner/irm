(ns irm.core
  (:require [irm.views :as v]
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

(defn- create-file-map [dir-str]
  (let [file-names (map str (.list (File. dir-str)))
        files (map #(File. %) file-names)]
    (->> files
        (map #(vector (str (.getName %))
                      {:selected? false
                       :depth 0
                       :directory? (boolean (.isDirectory %))
                       :open? false}))
        (into {}))))

(defn- create-screen [type]
  (let [file-map (create-file-map (current-dir))
        scr (s/get-screen type)]
    (s/start scr)
    (v/draw-file-screen scr file-map (current-dir))
    {:screen scr :file-map file-map}))

(defn- get-file-by-index [file-map index]
  (get (vec (sort file-map)) index))

(defn- select-file
  [{:keys [y]} file-map]
  (let [index (dec y)
        [file {selected :selected?}] (get-file-by-index file-map index)]
    (assoc-in file-map [file :selected?] (not selected))))

(defn- toggle-directory
  [{:keys [y]} file-map]
  (let [index (dec y)
        [file {open :open?}] (get-file-by-index file-map index)]
    (assoc-in file-map [file :open?] (not open))))

(defn- delete-files
  [file-map]
  (doall (->> file-map
             (filter #(-> % second :selected?))
             (map #(.delete (File. (first %)))))))

(defn- handle-input [scr file-map]
  (case (s/get-key-blocking scr)
    (:down \j \n) (do (update-cursor scr 0 1) [file-map true])
    (:up \k \p) (do (update-cursor scr 0 -1) [file-map true])
    \c (let [new-file-map (select-file @*cursor file-map)]
         (v/draw-file-screen scr new-file-map (current-dir))
         [new-file-map true])
    \x (do (delete-files file-map) [(v/draw-file-screen scr (create-file-map (current-dir)) (current-dir)) true])
    \q [file-map false]
    \r [(v/draw-file-screen scr (create-file-map (current-dir)) (current-dir)) true]
    \? (do
         (v/draw-help-screen scr)
         (s/get-key-blocking scr)
         (v/draw-file-screen scr file-map (current-dir))
         [file-map true])
    :tab (let [new-file-map (toggle-directory @*cursor file-map)]
           (v/draw-file-screen scr new-file-map (current-dir))
           [new-file-map true])
    [file-map true]))

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
