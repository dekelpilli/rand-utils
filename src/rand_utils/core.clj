(ns rand-utils.core
  (:require [data.deque :as dq]))

(defn weightings->probabilities [ws]
  (let [total (reduce + 0 ws)]
    (mapv #(/ % total) ws)))

(defn alias-generator
  "Performs the initialisation for Vose's Alias Method and returns a function that generates values based on the weightings."
  ([weightings-map] (alias-generator (keys weightings-map) (vals weightings-map)))
  ([values weightings]
   (assert (= (count weightings) (count values)))
   (let [values (vec values)
         probabilities (weightings->probabilities weightings)
         n (count probabilities)
         avg (/ 1 n)
         base (vec (repeat n nil))
         indexes (reduce
                   (fn [acc i]
                     (let [p (nth probabilities i)]
                       (update acc (if (>= p avg) :large :small) dq/add-first i)))
                   {:large dq/EMPTY :small dq/EMPTY}
                   (range n))]
     (loop [probabilities probabilities
            alias base
            probability base
            {:keys [large small] :as indexes} indexes]
       (cond
         (and (seq large) (seq small)) (let [less (dq/peek-last small)
                                             more (dq/peek-last large)
                                             p-of-less (* n (nth probabilities less))
                                             p-of-more (- (+ (nth probabilities more) (nth probabilities less))
                                                          avg)]
                                         (recur (assoc probabilities more p-of-more)
                                                (assoc alias less more)
                                                (assoc probability less p-of-less)
                                                (-> indexes
                                                    (update :large dq/remove-last)
                                                    (update :small dq/remove-last)
                                                    (update (if (>= p-of-more avg) :large :small) dq/add-last more))))
         (seq large) (recur probabilities alias
                            (assoc probability (dq/peek-last large) 1)
                            (update indexes :large dq/remove-last))
         (seq small) (recur probabilities alias
                            (assoc probability (dq/peek-last small) 1)
                            (update indexes :small dq/remove-last))
         :else (letfn [(generate
                         ([] (generate rand))
                         ([randomisation-function]
                          (let [i (int (* (randomisation-function) n))]
                            (nth values (if (<= (randomisation-function) (nth probability i))
                                          i
                                          (nth alias i))))))]
                 generate))))))

(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #(<= % r) w)))))
