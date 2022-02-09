(ns irm.core
  (:require 
    [clojure.java.io :as io]
    [malli.core :as m]
    [malli.experimental :as mx]
    [irm.registry :as r]
    [malli.util :as mu]
    [malli.dev :as md]
    [irm.select :refer [select]]
    [clojure.string :as str]
    [irm.protocols :as p])
  (:import
    [java.io File]))

(r/update-custom-registry
  {::id        :uuid
   ::path      :string
   ::name      :string
   ::marked?   :boolean
   ::dir?      :boolean
   ::raw-file  :file
   ::parent-id :uuid
   ::depth     :int})

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

(def AppState
  [:map
   [::files [:sequential FileEntry]]
   [::y :int]])

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

(mx/defn FileEntry->string
  [{::keys [depth dir? name marked?]} :- FileEntry]
  (let [left-pad (str/join "" (repeat (* depth 3) " "))
        icon     (if dir?
                   (if marked? " v  " " >  ")
                   (if marked? "[X] " "[ ] "))]
    (format "%s%s%s\n" left-pad icon name)))

(mx/defn sort-files :- [:sequential FileEntry]
  [files :- [:sequential FileEntry]]
  (sort-by
    (juxt #(not (::dir? %)) ::name)
    files))

(mx/defn FileEntries->strings
  [files :- [:sequential FileEntry]]
  (let [sorted-files (sort-files files)]
    (map FileEntry->string sorted-files)))

(mx/defn file-entries :- [:sequential FileEntry]
  [path :- [:or :file :string]
   fs :- :filesystem]
  (map File->FileEntry (p/files-in-dir fs path)))

(mx/defn mark-file :- AppState
  [{::keys [files y] :as state} :- AppState
   fs :- :filesystem]
  (let [file (get (vec (sort-files files)) y)
        updated-state (update state ::files #(map (fn [f]
                                                    (if (= (::id f) (::id file))
                                                      (update f ::marked? not)
                                                      f)) %))]
    (if-not (::dir? file)
      updated-state
      (update updated-state ::files #(concat % (file-entries (::raw-file file) fs))))))

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
        (printf "%s%s%s\n" left-pad icon name)))))