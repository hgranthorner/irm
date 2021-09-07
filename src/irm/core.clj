(ns irm.core
  (:require [irm.views :as v]
            [irm.utils :as u]
            [lanterna.screen :as s]
            [clojure.string :as st])
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
  (def ^:dynamic *dir-str* dir-str)
  (let [file-names (map #(str dir-str "/" %) (.list (File. dir-str)))
        files (map #(File. %) file-names)]
    (def ^:dynamic *files* files)
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

(defn- get-file-path-by-index [file-map index]
  (nth (u/file-map->paths file-map) index))

(defn- select-file
  [{:keys [y]} file-map]
  (let [index (dec y)
        path (get-file-path-by-index file-map index)
        {selected :selected?} (u/get-in-file-map file-map path)]
    (u/assoc-in-file-map file-map path :selected? (not selected))))

(defn- toggle-directory
  [{:keys [y]} file-map]
  (let [index (dec y)
        path (nth (u/file-map->paths file-map) index)]
    (def ^:dynamic *path* path)
    (-> (u/update-in-file-map file-map path :open? #(not %))
        (u/assoc-in-file-map path :children (create-file-map (str (current-dir) "/" (st/join "/" path)))))))

(assoc-in {:a {:b {:c 1}}} (flatten [[:a :b] :c]) 2)

(defn- delete-files
  [file-map]
  (doall (->> file-map
              (filter #(-> % second :selected?))
              (map #(.delete (File. (first %)))))))

(defn- handle-input [scr file-map]
  (def ^:dynamic *fm* file-map)
  (let [draw-fn (fn [fm]
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
    (try
      (event-loop scr file-map)
      (finally
        (s/stop scr)))))

(comment
  (get-in *fm* (interpose :children (nth)
                             (u/file-map->paths *fm*)
                             6))
  (.delete (File. "example.txt"))

  (def ^:dynamic *term* (future (-main "dev")))

  v/*draw-paths*

  (get-in *fm* (last v/*draw-paths*))

  @*term*
  *dir-str*
  (map str (.list (File. *dir-str*)))
  *fm*
  *path*
  *files*
  u/*assoc-path*
  (str (current-dir) "/" (st/join "/" *path*))
  (u/file-map->paths *fm*)
  (.getCanonicalPath (second *files*))
  (.isDirectory (second *files*))

  (nth (u/file-map->paths *fm*) 3)
  (get-file-path-by-index *fm* 3)

  (let [paths (u/file-map->paths *fm*)
        path (second paths)]
    (println path)
    (get-in *fm* path))
  (comment))
