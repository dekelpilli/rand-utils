(ns randy.core
  #?(:clj (:import (java.util Random)
                   (java.security SecureRandom))))

(defprotocol RandomNumberGenerator
  (next-int [_ n])
  (next-double [_] [_ lower]))

#?(:clj
   (extend-protocol RandomNumberGenerator
     Random
     (next-int [this n] (.nextInt this n))
     (next-double
       ([this] (.nextDouble this))
       ([this n] (.nextDouble this n)))))

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
   (assert (or (nil? values)
               (= (count weightings) (count values))))
   (let [values (when values (vec values))
         probabilities (weightings->probabilities weightings)
         n (count probabilities)
         avg (/ 1 n)
         base (vec (repeat n nil))
         indexes (reduce
                   (fn [acc i]
                     (let [p (nth probabilities i)]
                       (update acc (if (>= p avg) :large :small) #(cons i %))))
                   {}
                   (range n))]
     (loop [probabilities probabilities
            alias base
            probability base
            {:keys [large small] :as indexes} indexes]
       (cond
         (and large small) (let [less (first small)
                                             more (first large)
                                             p-of-less (* n (nth probabilities less))
                                             p-of-more (- (+ (nth probabilities more) (nth probabilities less))
                                                          avg)]
                                         (recur (assoc probabilities more p-of-more)
                                                (assoc alias less more)
                                                (assoc probability less p-of-less)
                                                (-> indexes
                                                    (update :large next)
                                                    (update :small next)
                                                    (update (if (>= p-of-more avg) :large :small) #(cons more %)))))
         small (recur probabilities alias
                            (assoc probability (first small) 1)
                            (update indexes :small next))
         large (recur probabilities alias
                            (assoc probability (first large) 1)
                            (update indexes :large next))
         :else (letfn [(generate-index
                         ([] (generate-index default-rng))
                         ([rng]
                          (let [i (next-int rng n)]
                            (if (<= (next-double rng) (nth probability i))
                              i
                              (nth alias i)))))]
                 (cond->> generate-index
                          values (comp #(nth values %)))))))))

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
