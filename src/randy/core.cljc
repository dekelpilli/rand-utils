(ns randy.core
  #?(:clj (:import (java.util Random)
                   (java.security SecureRandom))))

(defprotocol RandomNumberGenerator
  (next-int [this] [this upper] [this lower upper])
  (next-double [this] [this upper] [this lower upper]))

#?(:clj
   (extend-protocol RandomNumberGenerator
     Random
     (next-int
       ([this] (.nextInt this))
       ([this upper] (.nextInt this upper))
       ([this lower upper] (.nextInt this lower upper)))
     (next-double
       ([this] (.nextDouble this))
       ([this upper] (.nextDouble this upper))
       ([this lower upper] (.nextDouble this lower upper)))))

(def default-rng
  #?(:cljs
     (reify RandomNumberGenerator
       (next-int [_] (rand-int js/MAX_SAFE_INTEGER_))
       (next-int [_ upper] (rand-int upper))
       (next-int [_ lower upper] (+ lower (rand-int (- upper lower))))
       (next-double [_] (rand))
       (next-double [_ lower] (rand lower))
       (next-double [_ lower upper] (+ lower (rand (- upper lower)))))
     :clj (Random.)))

(defn weightings->probabilities [ws]
  (let [total (reduce + 0 ws)]
    (mapv #(/ % total) ws)))

(defn alias-method-sampler
  "Performs the initialisation for Vose's Alias Method and returns a function that generates values based on the weightings.
   The function returns indexes when values is null."
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

(defn sample
  ([coll] (sample default-rng coll))
  ([rng coll]
   (let [coll (vec coll)]
     (nth coll (next-int rng (count coll))))))
