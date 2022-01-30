(ns irm.core
  (:require [irm.select :refer [select]]
            [malli.experimental :as mx]
            [malli.dev :as md]
            [malli.util :as mu]
            [malli.registry :as mr]
            [malli.core :as m]))

(mr/set-default-registry!
  (merge
    (m/default-schemas)
    {::a :int}))

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

(thing 1 {::a 3} 3)

(-> (mu/optional-keys Thing)
  (mu/required-keys [:c]))

(comment
  (md/start!)
  )