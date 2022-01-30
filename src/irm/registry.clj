(ns irm.registry
  (:require [malli.core :as m]
            [malli.registry :as mr]))

(defonce custom-registry (atom (m/default-schemas)))

(defn update-custom-registry
  [m]
  (swap! custom-registry merge m)
  (mr/set-default-registry! @custom-registry))
