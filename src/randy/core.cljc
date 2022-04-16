(ns randy.core
  (:refer-clojure :rename {shuffle core-shuffle})
  #?(:cljs (:require [goog.array :as garray]))
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

(defn shuffle
  ([coll] (shuffle default-rng coll))
  ([rng coll]
   #?(:clj  (let [al (java.util.ArrayList. ^java.util.Collection coll)]
              (java.util.Collections/shuffle
                al
                (if (instance? Random rng)
                  rng
                  (proxy
                    [Random] []
                    (nextInt [_ upper] (next-int rng upper)))))
              (clojure.lang.RT/vector (.toArray al)))
      :cljs (let [a (to-array coll)]
              (garray/shuffle a #(next-double rng))
              (vec a)))))

(defn- take-transient [rng n coll]
  (take n
        ((fn shuffle [coll]
           (lazy-seq
             (let [c (count coll)]
               (when-not (zero? c)
                 (let [n (next-int rng c)]
                   (cons (get coll n)
                         (shuffle (pop! (assoc! coll n (get coll (dec c)))))))))))
         (transient coll))))

(defn sample-without-replacement
  ([n coll] (sample-without-replacement default-rng n coll))
  ([rng n coll]
   (let [coll (vec coll)
         size (count coll)]
     (if (> (/ n size) 2/11) ;TODO different algorithm is likely needed for cljs
       (subvec (shuffle rng coll) 0 n)
       (vec (take-transient rng n coll))))))
