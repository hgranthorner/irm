(ns irm.fs
  (:require
   [clojure.java.io :as io]
   [irm.registry :as r]
   [malli.core :as m]
   [irm.protocols :refer [FileSystem] :as p])
  (:import [java.io File]))

(r/update-custom-registry
 {:file (m/-simple-schema
         {:type            :user/file
          :pred            (partial instance? File)
          :type-properties {:error/message "should be a java.io.File"}})})

(def java-io-filesystem
  (reify FileSystem
    (current-dir [_] (io/file (System/getProperty "user.dir")))
    (files-in-dir [_this dir] 
      (if (instance? File dir)
        (.listFiles dir)
        (.listFiles (io/file dir))))))