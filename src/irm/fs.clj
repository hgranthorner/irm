(ns irm.fs
  (:require
   [clojure.java.io :as io]
   [malli.experimental :as mx]
   [irm.registry :as r]
   [malli.core :as m])
  (:import [java.io File]))

(r/update-custom-registry
 {:file (m/-simple-schema
         {:type            :user/file
          :pred            (partial instance? File)
          :type-properties {:error/message "should be a java.io.File"}})})

(defprotocol FileSystem
  (current-dir [_this] "Returns the current directory.")
  (files-in-current-dir [_this] "Returns the files in the current directory."))

(def java-io-filesystem
  (reify FileSystem
    (current-dir [_] (io/file (System/getProperty "user.dir")))
    (files-in-current-dir [this] (.listFiles (current-dir this)))))