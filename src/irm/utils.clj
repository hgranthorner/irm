(ns irm.utils)

(defn get-paths [root {:keys [open? children]}]
  (if open?
    (reduce (fn [acc [k m]]
              (conj acc (flatten [root (get-paths k m)])))
            [root] children)
    [root]))

(defn file-map->paths [fm]
  (->> (mapcat (fn [[k m]] (get-paths k m)) fm)
       (map #(if (seq? %) % [%]))))
