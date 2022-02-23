(ns rand-utils.core
  (:import (java.util ArrayDeque)
           (org.apache.commons.rng.sampling.distribution AliasMethodDiscreteSampler)
           (org.apache.commons.rng.core.source32 JDKRandom)))

(defn alias-generator
  "Performs the initialisation for Vose's Alias Method and returns a function that generates values based on the probabilities"
  ([probabilities-map] (alias-generator (keys probabilities-map) (vals probabilities-map)))
  ([values probabilities]
   (assert (= (count probabilities) (count values)))
   (let [values (vec values)
         probabilities (vec probabilities)
         n (count probabilities)
         avg (/ (reduce + probabilities) n)
         base (vec (repeat n nil))
         small (ArrayDeque.) ;TODO replace with clojure data structures for cljs support?
         large (ArrayDeque.)]
     (doseq [i (range n)]
       (let [p (nth probabilities i)]
         (-> (if (>= p avg) large small)
             (.add i))))
     (loop [alias base
            probability base
            probabilities probabilities]
       (cond
         (and (seq small) (seq large)) (let [less (.removeLast small)
                                             more (.removeLast large)
                                             p-of-less (* n (nth probabilities less))
                                             p-of-more (- (+ (nth probabilities more) (nth probabilities less))
                                                          avg)]
                                         (if (>= p-of-more (/ 1 n)) ;avg instead of 1/n?
                                           (.add large more)
                                           (.add small more))
                                         (recur (assoc alias less more)
                                                (assoc probability less p-of-less)
                                                (assoc probabilities more p-of-more)))
         ;TODO avg instead of 1? would need to update (rand) to be (* avg (rand))
         ;TODO remove (seq small) section because of stability granted by ratios instead of doubles?
         (seq small) (recur alias (assoc probability (.removeLast small) 1) probabilities)
         (seq large) (recur alias (assoc probability (.removeLast large) 1) probabilities)
         :else #(let [i (rand-int n)] ;TODO SecureRandom? Random via *binding*? Random via optional function param?
                  (nth values (if (<= (rand) (nth probability i))
                                i
                                (nth alias i)))))))))

(comment
  (let [m {"red" 2/10, "green" 5/10, "blue" 3/10}
        n 100000
        apache (AliasMethodDiscreteSampler/of
                 (JDKRandom. (System/currentTimeMillis))
                 (into-array Double/TYPE (map double (vals m))))
        apache-generator #(nth (vec (keys m))
                               (.sample apache))
        custom-generator (alias-generator m)]
    ;TODO benchmark
    {:apache (frequencies (repeatedly n apache-generator))
     :custom (frequencies (repeatedly n custom-generator))}))

(defn weightings->probabilities [ws]
  (let [total (reduce + 0 ws)]
    (map #(/ % total) ws)))

(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #(<= % r) w)))))
