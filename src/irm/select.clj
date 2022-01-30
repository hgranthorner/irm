(ns irm.select
  (:require [com.rpl.specter :as sp]
            [malli.util :as mu]
            [malli.core :as m]
            [malli.registry :as mr]))

(defn try-optional
  [x]
  (try
    (m/form (mu/optional-keys x))
    (catch Exception _
      x)))

(defn all-optional
  [xs]
  (mapv
    (fn [x]
      (if (sequential? x)
        (all-optional (try-optional x))
        x))
    (try-optional xs)))

(defn select
  "Recreates the basic functionality of clojure.alpha.spec/select for malli."
  [schema keys]
  (let [updated-schema (-> (all-optional schema)
                         (mu/required-keys (filter keyword? keys))
                         m/form)]
    (reduce
      (fn [acc m]
        (reduce
          (fn [acc [k vs]]
            (sp/transform [sp/ALL #(and (vector? %)
                                        (not-empty %)
                                        (= (first %) k))
                           sp/LAST]
              #(select % vs)
              (m/form acc)))
          acc
          m))
      updated-schema
      (filter map? keys))))

(comment
  (select [:map [:x :int]] [:x])
  (select [:map [:x :int]] [])
  (select [:map
           [:x :int]
           [:y [:map
                [:z :int]]]] [:y {:y [:z]}])
  (mr/set-default-registry!
    (merge (m/default-schemas)
      {::y :int}))
  (select [:map
           [:x :int]
           [:z [:map
                [:q [:map
                     ::y]]]]]
    [{:z [{:q [::y]}]}])
  )