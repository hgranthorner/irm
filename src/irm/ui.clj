(ns irm.ui
  (:require
    [irm.protocols :as p]
    [irm.fs :as fs]
    [irm.core :as c]
    [lanterna.screen :as s]
    [malli.experimental :as mx]
    [clojure.java.io :as io]
    [clojure.tools.trace :as trace])
  (:import
    [com.googlecode.lanterna.screen Screen]))

(def fs fs/java-io-filesystem)

(defn draw-screen
  [^Screen scr {::c/keys [y files] :as state}]
  (s/clear scr)
  (let [file-strs (c/FileEntries->strings files)]
    (doseq [[f i] (map vector file-strs (range))]
      (s/put-string scr 0 i f))
    (s/move-cursor scr 0 y)
    (s/redraw scr)
    state))

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
    :down (update state ::c/y inc-max (-> state ::c/files count dec))
    :up (update state ::c/y dec-min 0)
    :enter (c/mark-file state fs)
    (do
      (prn key)
      state)))

(defn get-handle-key
  []
  handle-key)

(defn get-draw-screen
  []
  draw-screen)

(def running (atom true))

(defn run-app
  ([^Screen scr]
   (run-app scr {::c/y     0
                 ::c/files (map c/File->FileEntry (p/files-in-dir fs (p/current-dir fs)))}))

  ([^Screen scr state]
   (def ^:dynamic s state)
   (let [draw-screen (get-draw-screen)
         _ (draw-screen scr state)
         key (s/get-key-blocking scr)
         handle-key (get-handle-key)]
     (when @running
       (case key
         \q nil
         (recur scr (handle-key state key)))))))

(defn -main
  [& args]
  (try
    (reset! running true)
    (let [prod? (empty? args)
          scr (s/get-screen (if prod? :text :swing))]
      (s/start scr)
      (run-app scr)
      (s/stop scr))
    (catch Exception e
      (reset! running false)
      (throw e))))

(comment
  (clojure.pprint/pprint (c/mark-file s))
  (malli.dev/start!)
  (malli.instrument/check)
  
  (clojure.pprint/pprint (::c/files s))
  (filter ::c/marked? (::c/files s))
  (filter ::c/marked? (::c/files (c/mark-file s)))
  (get-in (into [] (c/sort-files (::c/files s))) [(::c/y s) ::c/id])
  (reset! running false)
  (def foo (future (-main :dev)))
  (future-cancel foo)
  foo
  (def scr (s/get-screen :swing))

  (s/start scr)

  (s/move-cursor scr 1 1)
  (s/redraw scr)

  (s/stop scr)
  (s/clear scr)
  (type scr)

  (s/get-key-blocking scr))