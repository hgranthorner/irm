(ns irm.core
  (:require [clojure.java.io :as io]
            [irm.select :refer [select]]
            [irm.registry :as r]
            [malli.experimental :as mx]
            [malli.dev :as md]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.string :as str])
  (:import [java.io File]))

(r/update-custom-registry
  {::id        :uuid
   ::path      :string
   ::name      :string
   ::marked?   :boolean
   ::dir?      :boolean
   ::raw-file  :file
   ::parent-id :uuid
   ::depth     :int
   :file       (m/-simple-schema
                 {:type            :user/file
                  :pred            (partial instance? File)
                  :type-properties {:error/message "should be a java.io.File"}})})

(mx/defn current-dir :- :file
  "Returns the current directory as a java.io.File."
  []
  (io/file (System/getProperty "user.dir")))

(def FileEntry
  [:map
   ::raw-file
   ::id
   ::name
   ::path
   ::marked?
   ::dir?
   ::depth
   [::parent-id {:optional true}]])

(mx/defn File->FileEntry :- FileEntry
  ([file :- (mu/union :file :string)]
   (let [f (if (string? file)
             (io/file ^String file)
             file)]
     (assert (instance? File f))
     {::id       (random-uuid)
      ::path     (str f)
      ::name     (.getName f)
      ::marked?  false
      ::dir?     (.isDirectory f)
      ::raw-file f
      ::depth    0}))
  ([file :- (mu/union :file :string) {::keys [id depth]} :- (select FileEntry [::id ::depth])]
   (let [entry (File->FileEntry file)]
     (assoc entry ::parent-id id
                  ::depth (inc depth)))))

(comment
  (md/start!)
  (map #(File->FileEntry % {::id (random-uuid) ::depth 0})
    (.listFiles (current-dir)))
  (let [fs (map File->FileEntry
             (.listFiles (current-dir)))]
    (doseq [{::keys [depth dir? name marked?]} (update
                                                 (into [] (sort-by
                                                            (juxt #(not (::dir? %)) ::name)
                                                            fs))
                                                 9
                                                 #(-> (update % ::depth inc)
                                                    (update ::marked? not)))]
      (let [left-pad (str/join "" (repeat (* depth 3) " "))
            icon     (if dir?
                       (if marked? " v  " " >  ")
                       (if marked? "[X] " "[ ] "))]
        (printf "%s%s%s\n" left-pad icon name))))
  )