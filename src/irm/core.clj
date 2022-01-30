(ns irm.core
  (:require [irm.select :refer [select]]
            [irm.registry :as r]
            [malli.experimental :as mx]
            [malli.dev :as md]
            [malli.util :as mu]
            [malli.registry :as mr]
            [malli.core :as m]))

(r/update-custom-registry
  {::a :int})

(def Thing
  [:map
   [:c :int]
   [:d [:map
        ::a]]])

(mx/defn thing :- :int
  "Add two numbers"
  [x :- :int
   m :- (select Thing [{:b [:c]}])
   z :- :int]
  (let [{a ::a {c :c} :b} (merge m {::a 2 :b {:c 3}})]
    (+ x a z c)))

(m/validate (select Thing [{:d [::a]}])
  {:c 3
   :d {::a 5}})

(comment
  (md/start!)
  )