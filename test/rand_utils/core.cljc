(ns rand-utils.core
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]]
               :clj  [clojure.test :refer [deftest is testing]])
            [rand-utils.core :as sut]))

(defn- randomisation-variation-test [n f weightings]
  (let [probs (zipmap (keys weightings)
                      (sut/weightings->probabilities (vals weightings)))
        freqs (frequencies (repeatedly n f))]
    (doseq [[v prob] probs]
      (let [amount (get freqs v 0)
            expected-exact (* prob n)
            expected-lower-bound (* 0.99 expected-exact)
            expected-upper-bound (* 1.01 expected-exact)]
        (is (<= expected-lower-bound amount expected-upper-bound)
            (str amount " should be between " expected-lower-bound " and " expected-upper-bound))))))

(deftest weightings->probabilities-test
  (testing "maintains weighting ratio, total to 1"
    (is (= [2/10 5/10 3/10] (sut/weightings->probabilities [20 50 30])))
    (is (= [2/3 1/3] (sut/weightings->probabilities [1/2 1/4])))
    (is (= (every? ratio? (sut/weightings->probabilities [33 34 16 17 3]))))
    (is (= (every? double? (sut/weightings->probabilities [33.0 34.0 16.0 17.0 3.0]))))))

(defn- faux-randomiser [size index probability-roll]
  (let [x (.iterator (cycle [(double (/ index size)) probability-roll]))]
    #(.next x)))

(deftest alias-generator-test
  (testing "correctly prepares alias generator"
    (let [probabilities {"red" 20/100, "green" 50/100, "blue" 30/100, "blorange" 0}
          probabilities-map-generator (sut/alias-generator probabilities)]
      (randomisation-variation-test 1000000 probabilities-map-generator probabilities))
    (let [weightings {"red" 20, "green" 50, "blue" 30, "blorange" 0, "cyan" 5, "magenta" 19}
          weightings-colls-generator (sut/alias-generator (keys weightings) (vals weightings))]
      (randomisation-variation-test 1000000 weightings-colls-generator weightings))
    (let [m {"first" 1/2, "second" 1/5, "third" 1/10, "fourth" 3/10}
          size (count m)
          generator (sut/alias-generator m)]
      (is (= "first" (generator (faux-randomiser size 0 1.0))))
      (is (= "first" (generator (faux-randomiser size 0 0.0))))
      (is (= "second" (generator (faux-randomiser size 1 0.72))))
      (is (= "first" (generator (faux-randomiser size 1 0.73))))
      (is (= "third" (generator (faux-randomiser size 2 0.36))))
      (is (= "fourth" (generator (faux-randomiser size 2 0.37))))
      (is (= "fourth" (generator (faux-randomiser size 3 0.45))))
      (is (= "first" (generator (faux-randomiser size 3 0.46)))))))
