(ns ftw.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.core.matrix :as m]))


; reads in the csv file
(with-open [reader (io/reader "mygolfdata.csv")]
  (doall
    (csv/read-csv reader)))

; converts csv file to traditional dictionary structure
(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword) repeat)
       (rest csv-data)))

; stores golf-data as permanent variable
(def temp-data (csv-data->maps (with-open [reader (io/reader "mygolfdata.csv")]
                                 (doall
                                   (csv/read-csv reader)))))

;;Converts values to doubles, except for playernames which remain strings!
(defn vals-to-doubles [m]
  (zipmap (keys m)
          (map #(if (or (not %)
                        (= % "NA")
                        (clojure.string/includes? % " "))
                  (if (clojure.string/includes? % " ")
                    %
                    0.0)
                  (read-string %))
               (vals m))))

(def golf-data (map vals-to-doubles (into [] temp-data)))

(def t-data golf-data)

; extracts values from golf data and then converts into a vector of vectors
(defn myfunction [golf-data]
  (map #(into [] %) (map #(vals %) golf-data)))

; small function that swaps two elements in a vector
(defn swap [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))

; generalizes swap function to map
(defn swap-elements [v i1 i2]
  (map #(swap % i1 i2) v))

;; takes the log of the y variable for appropriate analysis
;; defines clojure log using Java log
(defn log [x] (Math/log x))

;; takes the log of the last variable
(defn log-last [data]
  (map #(conj (into [] (drop-last %)) (log (last %))) data))

;; takes the last variable
(defn nested-last [data]
  (map #(last %) data))

;; Final form of regression-data for TESTING (Includes playernames)

(def regression-data
  (swap-elements (swap-elements (swap-elements (swap-elements (log-last (swap-elements (myfunction (map #(dissoc % :TwoPutts :ThreePutts :Index :Season :AvgOfficialWGR)
                                            t-data)) 5 10)) 6 7) 7 8) 8 9) 9 10))

(comment
  "Variables are: DrivingAccuracy, PuttingAverage, Scrambling, DrivingDistance,
  SandSave, OnePutts, GreensFringeInReg, ProximityToHole, ScoringAvg, OfficialMoney")

;;take about 25% of the data for training!
(def training-data
  (random-sample 0.25 regression-data))

;;take about 75% of the data to be used for testing later
(def testing-data
  (random-sample 0.75 regression-data))

;; An individual will be an expression made of functions +, -, *, and
;; pd (protected division), along with terminals x and randomly chosen
;; constants between -5.0 and 5.0. Note that for this problem the
;; presence of the constants actually makes it much harder, but that
;; may not be the case for other problems.

;; We'll the functions and the arities in a map.

(def function-table (zipmap '(+ - * pd)
                            '(2 2 2 2 )))

(defn random-function
  []
  (rand-nth (keys function-table)))

(defn random-terminal
  []
  (rand-nth (list 'x1 'x2 'x3 'x4 'x5 'x6 'x7 'x8 'x9 (- (rand 40) 20))))

(defn random-code
  [depth]
  (if (or (zero? depth)
          (zero? (rand-int 2)))
    (random-terminal)
    (let [f (random-function)]
      (cons f (repeatedly (get function-table f)
                          #(random-code (dec depth)))))))

;; And we have to define pd (protected division):

(defn pd
  "Protected division; returns 0 if the denominator is zero."
  [num denom]
  (if (zero? denom)
    0
    (/ num denom)))

;; We can now evaluate the error of an individual by creating a function
;; built around the individual, calling it on all of the x values, and
;; adding up all of the differences between the results and the
;; corresponding y values.

(defn error
  [individual]
  (let [value-function (eval (list 'fn '[x1 x2 x3 x4 x5 x6 x7 x8 x9] individual))]
    (reduce + (map (fn [[x1 x2 x3 x4 x5 x6 x7 x8 x9 y]]
                     (Math/abs
                       (- (float (value-function x1 x2 x3 x4 x5 x6 x7 x8 x9)) y)))
                   training-data))))

(defn get-predictors [individual]
  (let [value-function (eval (list 'fn '[x1 x2 x3 x4 x5 x6 x7 x8 x9] individual))]
    (map (fn [[x1 x2 x3 x4 x5 x6 x7 x8 x9 y]]
           (value-function x1 x2 x3 x4 x5 x6 x7 x8 x9))
                   training-data)))

;; To help write mutation and crossover functions we'll write a utility
;; function that returns a random subtree from an expression and another that
;; replaces a random subtree of an expression.

(defn codesize [c]
  (if (seq? c)
    (count (flatten c))
    1))

(defn random-subtree
  [i]
  (if (zero? (rand-int (codesize i)))
    i
    (random-subtree
      (rand-nth
        (apply concat
               (map #(repeat (codesize %) %)
                    (rest i)))))))

;(random-subtree '(+ (* x (+ y z)) w))

(defn replace-random-subtree
  [i replacement]
  (if (zero? (rand-int (codesize i)))
    replacement
    (let [position-to-change
          (rand-nth
            (apply concat
                   (map #(repeat (codesize %1) %2)
                        (rest i)
                        (iterate inc 1))))]
      (map #(if %1 (replace-random-subtree %2 replacement) %2)
           (for [n (iterate inc 0)] (= n position-to-change))
           i))))

(defn mutate
  [i]
  (replace-random-subtree i (random-code 2)))

;(mutate '(+ (* x (+ y z)) w))

(defn crossover
  [i j]
  (replace-random-subtree i (random-subtree j)))

;; We'll also want a way to sort a population by error that doesn't require
;; lots of error re-computation:

(defn sort-by-error
  [population]
  (vec (map second
            (sort (fn [[err1 ind1] [err2 ind2]] (< err1 err2))
                  (map #(vector (error %) %) population)))))

;; Finally, we'll define a function to select an individual from a sorted
;; population using tournaments of a given size.

(defn select
  [population tournament-size]
  (let [size (count population)]
    (nth population
         (apply min (repeatedly tournament-size #(rand-int size))))))

;; Now we can evolve a solution by starting with a random population and
;; repeatedly sorting, checking for a solution, and producing a new
;; population.


(defn evolve
  [popsize]
  (println "Starting evolution...")
  (println "Testing, testing!")
  (loop [generation 0
         population (sort-by-error (repeatedly popsize #(random-code 3)))]
    (let [best (first population)
          best-error (error best)]
      (println "======================")
      (println "Generation:" generation)
      (println "Best program:" best)
      (println "Best error:" best-error)

      (println "     Median error:" (error (nth population
                                                (int (/ popsize 2)))))
      (println "     Average program size:"
               (float (/ (reduce + (map count (map flatten population)))
                         (count population))))
      (if (or (< best-error 250) (> generation 39)) ;; good enough to count as success
        best
        (recur
          (inc generation)
          (sort-by-error
            (concat
              (repeatedly (* 1/2 popsize) #(mutate (select population 5)))
              (repeatedly (* 1/4 popsize) #(crossover (select population 5)
                                                      (select population 5)))
              (repeatedly (* 1/4 popsize) #(select population 5)))))))))


;;Function for evaluating the result of the testing data
(defn evaluate []
  (let [best (evolve 1000)
        value-function (eval (list 'fn '[x1 x2 x3 x4 x5 x6 x7 x8 x9] best))]
    (map (fn [[x1 x2 x3 x4 x5 x6 x7 x8 x9 y]]
           (list (float (value-function x1 x2 x3 x4 x5 x6 x7 x8 x9))
                 y
                 (- (float (value-function x1 x2 x3 x4 x5 x6 x7 x8 x9)) y)))
         testing-data)))

(defn display-results []
  (let [errors (evaluate)]
    (interleave errors (nested-last testing-data))))

#_(display-results)