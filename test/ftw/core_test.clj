(ns ftw.core-test
  (:require [clojure.test :refer :all]
            [ftw.core :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn main- [args]
  (csv/read-csv "test"))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
