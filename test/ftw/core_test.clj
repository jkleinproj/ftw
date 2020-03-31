(ns ftw.core-test
  (:require [clojure.test :refer :all]
            [ftw.core :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn main- [args]
  (csv/read-csv "test"))

; reads in the csv file
(with-open [reader (io/reader "/Users/michaelbakshandeh/Downloads/stats230/projectdata/mygolfdata.csv")]
  (doall
    (csv/read-csv reader)))

; converts csv file to traditional dictionary structure
(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
       (rest csv-data)))

; runs the traditional dictionary structure on csv-data
(csv-data->maps (with-open [reader (io/reader "/Users/michaelbakshandeh/Downloads/stats230/projectdata/mygolfdata.csv")]
                  (doall
                    (csv/read-csv reader))))

; want to figure out how to get rid of the ': (row number)' column
; any ideas of how to do this in clojure??

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
