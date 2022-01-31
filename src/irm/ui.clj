(ns irm.ui
  (:require [irm.core :as c]
            [lanterna.screen :as s]
            [malli.experimental :as mx])
  (:import [com.googlecode.lanterna.screen Screen]))

(defn draw-screen
  [^Screen scr {::keys [y] :as state}]
  (s/clear scr)
  (s/move-cursor 0 y)
  (s/redraw scr)
  state)

(mx/defn inc-max :- :int
  [n :- :int max :- :int]
  (let [n' (inc n)]
    (if (<= max n')
      max
      n')))

(mx/defn dec-min :- :int
  [n :- :int min :- :int]
  (let [n' (dec n)]
    (if (<= n' min)
      min
      n')))

(defn handle-key
  [state key]
  (case key
    :down (update state ::c/y inc-max (count (::c/files state)))
    :up (update state ::c/y dec-min 0)
    (do
      (prn key)
      state)))

(defn run-app
  ([^Screen scr]
   (run-app scr {::c/y     0
                 ::c/files (map
                             c/File->FileEntry
                             (.listFiles
                               (c/current-dir)))}))

  ([^Screen scr state]
   (draw-screen scr state)
   (let [key (s/get-key-blocking scr)]
     (case key
       \q nil
       (recur scr (handle-key state key))))))

(defn -main
  [& args]
  (let [scr (s/get-screen (if (empty? args) :text :swing))]
    (s/start scr)
    π(run-app scr)
    (s/stop scr)))

(comment
  (future (-main :dev))
  (def scr (s/get-screen :swing))

  (s/start scr)

  (s/move-cursor scr 1 1)
  (s/redraw scr)

  (s/stop scr)
  (s/clear scr)
  (type scr)

  (s/get-key-blocking scr))