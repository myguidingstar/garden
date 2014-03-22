(ns garden.selectors-test
  (:refer-clojure :exclude [+ - >])
  (:require #+clj
            [clojure.test :refer :all]
            #+cljs
            [cemerick.cljs.test]
            [garden.selectors :as s])
  #+cljs (:require-macros [cemerick.cljs.test :refer [deftest is testing]]))

(deftest specificity-test
  (testing "specificity"
    (are [x y] (= (s/specificity x) y)
      "*" 0
      "li" 1
      "ul li" 2
      "ul ol+li" 3
      "h1 + *[REL=up]" 11
      "ul ol li.red" 13
      "li.red.level" 21
      "#x34y" 100
      "#s12:not(FOO)" 101)))
