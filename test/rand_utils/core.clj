(ns rand-utils.core
  (:require [clojure.test :refer [deftest testing is]]
            [rand-utils.core :as sut]))

(defn- randomisation-variation-test [n f weightings]
  (let [probs (zipmap (keys weightings)
                      (sut/weightings->probabilities (vals weightings)))
        freqs (frequencies (repeatedly n f))]
    (doseq [[v prob] probs]
      (let [amount (get freqs v 0)
            expected-exact (* prob n)
            expected-lower-bound (* 0.95 expected-exact)
            expected-upper-bound (* 1.05 expected-exact)]
        (is (>= amount expected-lower-bound))
        (is (<= amount expected-upper-bound))))))

(deftest weightings->probabilities-test
  (testing "maintains weighting ratio, total to 1"
    (is (= [2/10 5/10 3/10] (sut/weightings->probabilities [20 50 30])))
    (is (= (every? ratio? (sut/weightings->probabilities [33 34 16 17 3]))))
    (is (= (every? double? (sut/weightings->probabilities [33.0 34.0 16.0 17.0 3.0]))))))

(deftest alias-generator-test
  (testing "correctly prepares alias generator"
    (let [probabilities {"red" 20/100, "green" 50/100, "blue" 30/100, "blorange" 0}
          probabilities-map-generator (sut/alias-generator probabilities)
          weightings {"red" 20, "green" 50, "blue" 30, "blorange" 0, "cyan" 5, "magenta" 19}
          weightings-colls-generator (sut/alias-generator (keys weightings) (vals weightings))]
      (randomisation-variation-test 100000 probabilities-map-generator probabilities)
      (randomisation-variation-test 100000 weightings-colls-generator weightings)
      ;TODO add tests for 1-arity version with deterministic function provided
      )))
