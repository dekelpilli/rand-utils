(ns rand-utils.core
  (:import (java.util ArrayDeque)
           (org.apache.commons.rng.sampling.distribution AliasMethodDiscreteSampler)
           (org.apache.commons.rng.core.source32 JDKRandom)))

(defn alias-generator
  "Performs the initialisation for Vose's Alias Method and returns a function that generates values based on the weightings"
  ([weightings-map] (alias-generator (keys weightings-map) (vals weightings-map)))
  ([values weightings]
   (assert (= (count weightings) (count values)))
   (let [values (vec values)
         weightings (vec weightings)
         n (count weightings)
         total (reduce + weightings)
         avg (/ total n)
         base (vec (repeat n nil))
         small (ArrayDeque.) ;TODO replace with clojure data structures for cljs support?
         large (ArrayDeque.)]
     (doseq [i (range n)]
       (let [p (nth weightings i)]
         (-> (if (>= p avg) large small)
             (.add i))))
     (loop [alias base
            probability base
            weightings weightings]
       (cond
         (and (seq small) (seq large)) (let [less (.removeLast small)
                                             more (.removeLast large)
                                             p-of-less (* n (nth weightings less))
                                             p-of-more (- (+ (nth weightings more) (nth weightings less))
                                                          avg)]
                                         (if (>= p-of-more avg)
                                           (.add large more)
                                           (.add small more))
                                         (recur (assoc alias less more)
                                                (assoc probability less p-of-less)
                                                (assoc weightings more p-of-more)))
         (seq small) (recur alias (assoc probability (.removeLast small) total) weightings)
         (seq large) (recur alias (assoc probability (.removeLast large) total) weightings)
         :else (letfn [(generate
                         ([] (generate rand))
                         ([rand-f] (let [i (int (* (rand-f) n))]
                                     (nth values (if (<= (rand-f) (nth probability i))
                                                   i
                                                   (nth alias i))))))]
                 generate))))))

(comment
  (let [m {"red" 20, "green" 50, "blue" 30}
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
