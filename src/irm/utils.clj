(ns irm.utils)

(defn- get-paths [root {:keys [open? children]}]
  (if open?
    (reduce (fn [acc [k m]]
              (conj acc (flatten [root (get-paths k m)])))
            [root] children)
    [root]))

(defn file-map->paths [fm]
  (->> (mapcat (fn [[k m]] (get-paths k m)) fm)
       (map #(if (seq? %) (vec %) [%]))))

(defn get-in-file-map
  ([fm path]
   (get-in fm (interpose :children path)))
  ([fm path k]
   (get-in fm (flatten [(interpose :children path) k]))))

(defn assoc-in-file-map
  ([fm path v]
   (assoc-in fm (interpose :children path) v))
  ([fm path k v]
   (def ^:dynamic *assoc-path* (flatten [(interpose :children path) k]))
   (assoc-in fm (flatten [(interpose :children path) k]) v)))


(defn update-in-file-map
  ([fm path f]
   (update-in fm (interpose :children path) f))
  ([fm path k f]
   (def ^:dynamic *update-path* (flatten [(interpose :children path) k]))
   (update-in fm (flatten [(interpose :children path) k]) f)))
