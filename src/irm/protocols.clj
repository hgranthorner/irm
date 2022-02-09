(ns irm.protocols
  (:require
   [irm.registry :as r]
   [malli.core :as m]))

(defprotocol FileSystem
  (current-dir [_this] "Returns the current directory.")
  (files-in-dir [_this dir] "Returns the files in the directory."))

(r/update-custom-registry
 {:filesystem (m/-simple-schema
               {:type            :user/filesystem
                :pred            (partial satisfies? FileSystem)
                :type-properties {:error/message "should satisty the irm.protocols/FileSystem protocol."}})})