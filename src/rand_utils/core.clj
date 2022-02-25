(ns rand-utils.core
  (:require [data.deque :as dq]))

(defn alias-generator
  "Performs the initialisation for Vose's Alias Method and returns a function that generates values based on the weightings."
  ([weightings-map] (alias-generator (keys weightings-map) (vals weightings-map)))
  ([values weightings]
   (assert (= (count weightings) (count values)))
   (let [values (vec values)
         weightings (vec weightings)
         n (count weightings)
         total (reduce + weightings)
         avg (/ total n)
         base (vec (repeat n nil))
         indexes (reduce
                   (fn [acc i]
                     (let [p (nth weightings i)]
                       (update acc (if (>= p avg) :large :small) dq/add-first i)))
                   {:large dq/EMPTY :small dq/EMPTY}
                   (range n))]
     (loop [weightings weightings
            alias base
            probability base
            {:keys [large small] :as indexes} indexes]
       (cond
         (and (seq large) (seq small)) (let [less (dq/peek-last small)
                                             more (dq/peek-last large)
                                             p-of-less (* n (nth weightings less))
                                             p-of-more (- (+ (nth weightings more) (nth weightings less))
                                                          avg)]
                                         (recur (assoc weightings more p-of-more)
                                                (assoc alias less more)
                                                (assoc probability less p-of-less)
                                                (-> indexes
                                                    (update :large dq/remove-last)
                                                    (update :small dq/remove-last)
                                                    (update (if (>= p-of-more avg) :large :small) dq/add-last more))))
         (seq large) (recur weightings alias
                            (assoc probability (dq/peek-last large) total)
                            (update indexes :large dq/remove-last))
         (seq small) (recur weightings alias
                            (assoc probability (dq/peek-last small) total)
                            (update indexes :small dq/remove-last))
         :else (letfn [(generate
                         ([] (generate rand))
                         ([randomisation-function]
                          (let [i (int (* (randomisation-function) n))]
                            (nth values (if (<= (* (randomisation-function) total) (nth probability i))
                                          i
                                          (nth alias i))))))]
                 generate))))))

(defn weightings->probabilities [ws]
  (let [total (reduce + 0 ws)]
    (map #(/ % total) ws)))

(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #(<= % r) w)))))
