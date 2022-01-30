(ns irm.select
  (:require [clojure.test :refer :all]
            [irm.select :refer [select]]
            [malli.core :as m]
            [malli.registry :as mr]))

(deftest select-tests
  (testing "basic cases"
    (is
      (= (select [:map [:x :int]] [])
        [:map [:x {:optional true} :int]]))
    (is
      (= (select [:map [:x :int]] [:x])
        [:map [:x :int]])))
  (testing "nesting"
    (is
      (=
       (select [:map
                [:x :int]
                [:y [:map
                     [:z :int]]]] [:y {:y [:z]}])
        [:map [:x {:optional true} :int] [:y [:map [:z :int]]]])))
  (testing "deeply nested namespace keys"
    (mr/set-default-registry!
      (merge (m/default-schemas)
        {::y :int}))
    (is
      (=
       (select [:map
                [:x :int]
                [:z [:map
                     [:q [:map
                          ::y]]]]]
         [{:z [{:q [::y]}]}])
        [:map
         [:x {:optional true} :int]
         [:z {:optional true} [:map [:q {:optional true} [:map [:irm.select/y :irm.select/y]]]]]]))))

(comment
  (run-tests)
  )
