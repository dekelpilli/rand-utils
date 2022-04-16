(ns randy.swr-perf
  (:require [clojure.test :refer [deftest is testing]]
            [randy.core :as r]
            [incanter.core :as i]
            [incanter.charts :as ic]))

;modified/modernised from https://gist.github.com/pepijndevos/805747


(defn take-shuffle [_ n coll]
  (subvec (shuffle coll) 0 n))

(defn take-filtered [rng n coll]
  (let [coll (vec coll)]
    (into [] (comp (distinct) (take n)) (repeatedly #(r/sample rng coll)))))

; reduce, reorder, subvec, O(m)
(defn take-reduce [rng n coll]
  (let [len (count coll)]
    (subvec (->> (range n) ;TODO use reduce-kv on coll to avoid range lazy-seq
                 (into [] (map (fn [n] [n (r/next-int rng n len)])))
                 (reduce
                   (fn swap [v pair]
                     (let [i (first pair)
                           b (second pair)]
                       (assoc v b (get v i) i (get v b))))
                   coll))
            0 n)))

;TODO below
; amalloy, O(m)
(defn take-iterate [num coll]
  (first
    (nth
      (iterate (fn [[ret candidates]]
                 (let [idx (rand-int (count candidates))]
                   [(conj ret (candidates idx))
                    (subvec (assoc candidates idx (candidates 0))
                            1)]))
               [[]
                coll])
      num)))

; amalloy, o(mg)
(defn take-transient [nr coll]
  (take nr
        ((fn shuffle [coll]
           (lazy-seq
             (let [c (count coll)]
               (when-not (zero? c)
                 (let [n (rand-int c)]
                   (cons (get coll n)
                         (shuffle (pop! (assoc! coll n (get coll (dec c)))))))))))
         (transient coll))))

(defn time-f [f]
  (let [start (. System (nanoTime))]
    (f)
    (- (. System (nanoTime)) start)))

(defn plot-len [f n]
  (let [coll (vec (range n))]
    (time-f #(doall (f 1000 coll)))))

(defn plot-take [f n]
  (let [coll (vec (range 100000))]
    (time-f #(doall (f n coll)))))

(def x (range 1000 100000 1000))

(defn points [f g]
  (System/gc)
  (map #(f g %) x))

(defn draw-line [plot plotfn randfn name]
  (ic/add-points plot x (points plotfn randfn) :series-label name))

(defn do-it []
  (-> (ic/scatter-plot [] [] :legend true)
      (draw-line plot-len take-shuffle "shuffle")
      (draw-line plot-len take-filtered "filtered")
      (draw-line plot-len take-reduce "reduce")
      (draw-line plot-len take-iterate "iterate")
      (draw-line plot-len take-transient "transient")
      (i/save "len.png"))

  (-> (ic/scatter-plot [] [] :legend true)
      (draw-line plot-take take-shuffle "shuffle")
      (draw-line plot-take take-filtered "filtered")
      (draw-line plot-take take-reduce "reduce")
      (draw-line plot-take take-iterate "iterate")
      (draw-line plot-take take-transient "transient")
      (i/save "take.png")))
