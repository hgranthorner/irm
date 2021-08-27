(ns irm.core
  (:require [lanterna.screen :as s])
  (:import [java.io File])
  (:gen-class))



(defn- create-screen [type]
  (let [current-path (.getCanonicalPath (File. "."))
        scr (s/get-screen type)]
    (s/start scr)

    (s/put-string scr 10 10 "Hello, world!")
    (s/put-string scr 10 11 (str "Current directory: " current-path))
    (s/redraw scr)
    scr))

    

(defn -main [& args]
  (cond
    (not (nil? args)) (let [x (create-screen :text)]
                        (s/get-key-blocking x)
                        (s/stop x))
    :else  (.println System/out "needs argument to start")))

(comment
  (def *scr* (create-screen :swing))
  (map-indexed (fn [i f]
                 (s/put-string *scr* 10 (+ 12 i) f))
   (map str (.list (File. "."))))
  (s/redraw *scr*)
  (comment))
