(ns randy.core_test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]]
               :clj  [clojure.test :refer [deftest is testing]])
            [randy.core :as sut]
            [randy.rng :as rng]))

(defn- randomisation-variation-test [n f tolerance weightings]
  (let [probs (zipmap (keys weightings)
                      (sut/weightings->probabilities (vals weightings)))
        freqs (frequencies (repeatedly n f))]
    (doseq [[v prob] probs]
      (let [amount (get freqs v 0)
            expected-exact (* prob n)
            expected-lower-bound (* (- 1 tolerance) expected-exact)
            expected-upper-bound (* (+ 1 tolerance) expected-exact)]
        (is (<= expected-lower-bound amount expected-upper-bound)
            (str amount " should be between " expected-lower-bound " and " expected-upper-bound))))))

(deftest weightings->probabilities-test
  (testing "maintains weighting ratio, total to 1"
    (is (= [2/10 5/10 3/10] (sut/weightings->probabilities [20 50 30])))
    (is (= [2/3 1/3] (sut/weightings->probabilities [1/2 1/4])))
    (is (every? ratio? (sut/weightings->probabilities [33 34 16 17 3])))
    (is (every? double? (sut/weightings->probabilities [33.0 34.0 16.0 17.0 3.0])))))


(defn- alias-faux-randomiser [index probability-roll]
  (reify rng/RandomNumberGenerator
    (next-int [_ _] index)
    (next-double [_] probability-roll)))

(deftest alias-method-sampler-test
  (testing "correctly prepares alias generator"
    (let [probabilities {"red" 20/100, "green" 50/100, "blue" 30/100, "blorange" 0}
          probabilities-map-generator (sut/alias-method-sampler probabilities)]
      (randomisation-variation-test 500000 probabilities-map-generator 0.01 probabilities))
    (let [weightings {"red" 20, "green" 50, "blue" 30, "blorange" 0, "cyan" 5, "magenta" 19}
          weightings-colls-generator (sut/alias-method-sampler (keys weightings) (vals weightings))]
      (randomisation-variation-test 500000 weightings-colls-generator 0.01 weightings))
    (let [generator (sut/alias-method-sampler {"first" 1/2, "second" 1/5, "third" 1/10, "fourth" 3/10})]
      (is (= "first" (generator (alias-faux-randomiser 0 1.0))))
      (is (= "first" (generator (alias-faux-randomiser 0 0.0))))
      (is (= "second" (generator (alias-faux-randomiser 1 0.72))))
      (is (= "first" (generator (alias-faux-randomiser 1 0.73))))
      (is (= "third" (generator (alias-faux-randomiser 2 0.36))))
      (is (= "fourth" (generator (alias-faux-randomiser 2 0.37))))
      (is (= "fourth" (generator (alias-faux-randomiser 3 0.45))))
      (is (= "first" (generator (alias-faux-randomiser 3 0.46)))))
    (let [generator (sut/alias-method-sampler nil [1/2 1/5 1/10 3/10])]
      (is (= 0 (generator (alias-faux-randomiser 0 1.0))))
      (is (= 0 (generator (alias-faux-randomiser 0 0.0))))
      (is (= 1 (generator (alias-faux-randomiser 1 0.72))))
      (is (= 0 (generator (alias-faux-randomiser 1 0.73))))
      (is (= 2 (generator (alias-faux-randomiser 2 0.36))))
      (is (= 3 (generator (alias-faux-randomiser 2 0.37))))
      (is (= 3 (generator (alias-faux-randomiser 3 0.45))))
      (is (= 0 (generator (alias-faux-randomiser 3 0.46)))))))

(defn- weighted-sample-faux-randomiser [expected-total probability-roll]
  (reify rng/RandomNumberGenerator
    (next-double [_ n]
      (is (= expected-total n))
      probability-roll)))

(deftest weighted-sample-test
  (testing "correctly selects a random sample"
    (let [probabilities {"red" 20/100, "green" 50/100, "blue" 30/100, "blorange" 0}
          probabilities-map-generator #(sut/weighted-sample probabilities)]
      (randomisation-variation-test 100000 probabilities-map-generator 0.05 probabilities))
    (let [weightings {"red" 20, "green" 50, "blue" 30, "blorange" 0, "cyan" 5, "magenta" 19}
          weightings-colls-generator #(sut/weighted-sample weightings)]
      (randomisation-variation-test 100000 weightings-colls-generator 0.05 weightings))
    (let [weightings {"first" 1, "second" 2, "third" 3, "fourth" 4}
          weightings-faux-randomiser (partial weighted-sample-faux-randomiser 10)]
      (is (= "first" (sut/weighted-sample (weightings-faux-randomiser 0.99) weightings)))
      (is (= "first" (sut/weighted-sample (weightings-faux-randomiser 0.0) weightings)))
      (is (= "second" (sut/weighted-sample (weightings-faux-randomiser 1.0) weightings)))
      (is (= "second" (sut/weighted-sample (weightings-faux-randomiser 2.99) weightings)))
      (is (= "third" (sut/weighted-sample (weightings-faux-randomiser 3.0) weightings)))
      (is (= "third" (sut/weighted-sample (weightings-faux-randomiser 5.99) weightings)))
      (is (= "fourth" (sut/weighted-sample (weightings-faux-randomiser 6.0) weightings)))
      (is (= "fourth" (sut/weighted-sample (weightings-faux-randomiser 9.99) weightings))))))
