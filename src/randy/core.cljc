(ns randy.core
  (:require [data.deque :as dq])
  #?(:clj (:import (java.util Random)
                   (java.security SecureRandom))))

(defprotocol RandomNumberGenerator
  (next-int [_ n])
  (next-double [_] [_ lower]))

#?(:clj
   (extend-protocol RandomNumberGenerator
     Random
     (next-int [this n] (.nextInt this n))
     (next-double [this] (.nextDouble this))
     (next-double [this n] (.nextDouble this n))))

(def default-rng
  #?(:cljs
     (reify RandomNumberGenerator
       (next-int [_ n] (rand-int n))
       (next-double [_] (rand))
       (next-double [_ n] (rand n)))
     :clj (SecureRandom.)))

(defn weightings->probabilities [ws]
  (let [total (reduce + 0 ws)]
    (mapv #(/ % total) ws)))

(defn alias-method-sampler
  "Performs the initialisation for Vose's Alias Method and returns a function that generates values based on the weightings."
  ([weightings-map] (alias-method-sampler (keys weightings-map) (vals weightings-map)))
  ([values weightings]
   (assert (= (count weightings) (count values))) ;TODO allow nil values to simply return indexes?
   (let [values (vec values)
         probabilities (weightings->probabilities weightings)
         n (count probabilities)
         avg (/ 1 n)
         base (vec (repeat n nil)) ;TODO use arrays for faster fetches (aget vs nth)
         indexes (reduce
                   (fn [acc i]
                     (let [p (nth probabilities i)]
                       (update acc (if (>= p avg) :large :small) dq/add-last i)))
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
         (seq small) (recur probabilities alias
                            (assoc probability (dq/peek-last small) 1)
                            (update indexes :small dq/remove-last))
         (seq large) (recur probabilities alias
                            (assoc probability (dq/peek-last large) 1)
                            (update indexes :large dq/remove-last))
         :else (letfn [(sample
                         ([] (sample default-rng))
                         ([rng]
                          (let [i (next-int rng n)]
                            (nth values (if (<= (next-double rng) (nth probability i))
                                          i
                                          (nth alias i))))))]
                 sample))))))

(defn weighted-sample
  ([m] (weighted-sample default-rng m))
  ([rng m]
   (let [w (reductions #(+ % %2) (vals m))
         r (next-double rng (last w))]
     (nth (keys m) (count (take-while #(<= % r) w))))))

;https://gist.github.com/pepijndevos/805747 TODO consider
(defn sample-n-unique
  ([n coll] (sample-n-unique default-rng n coll))
  ([rng n coll]
   (let [coll (vec coll)
         size (count coll)
         indexes (if (> n (/ size 2)) ;TODO proper algorithm for deciding optimal strategy
                   (let [possible-indexes (set (range size))]
                     )
                   (loop [indexes #{}
                          ordered []]
                     (let [index (next-int rng size)
                           new (conj indexes index)]
                       (if (identical? new indexes)
                         (recur indexes ordered)
                         (cond-> new
                                 (< n (count new)) (recur (conj ordered index)))))))]
     (map #(nth coll %) indexes))))
