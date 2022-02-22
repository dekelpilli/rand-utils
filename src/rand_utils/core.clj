(ns rand-utils.core)

(defn alias-generator
  "Performs the initialisation for Vose's Alias Method and returns a function that generates values based on the probabilities"
  ([probabilities-map] (alias-generator (keys probabilities-map) (vals probabilities-map)))
  ([values probabilities]
   (let [values (vec values)
         n (count probabilities)
         base (vec (repeat n nil))
         scaled-probs (mapv #(* n %) probabilities)
         {:keys [small large]} (reduce
                                 (fn [acc i]
                                   (let [p (nth scaled-probs i)]
                                     (update acc (if (< p 1) :small :large) conj i)))
                                 {:small [] :large []}
                                 (range n))]
     (loop [small small
            large large
            alias base
            prob base]
       (cond
         (and (seq small) (seq large)) (let [l (first small)
                                             p-of-l (nth scaled-probs l)
                                             g (first large)
                                             p-of-g (- (+ (nth scaled-probs g) p-of-l)
                                                       1)
                                             [small large] (if (< p-of-g 1)
                                                             [(conj small g) large]
                                                             [small (conj large g)])]
                                         (recur (rest small)
                                                (rest large)
                                                (assoc alias l g)
                                                (assoc prob l p-of-l)))
         (seq large) (recur nil (rest large) alias (assoc prob (first large) 1))
         (seq small) (recur (rest small) nil alias (assoc prob (first small) 1))
         :else #(let [i (rand-int n)]
                  (nth values (if (<= (rand) (nth prob i))
                                i
                                (nth alias i)))))))))

(comment
  ((alias-generator ["orage" "yellow" "green" "cyan" "grey" "blue" "pink"]
                    [1/8, 1/5, 1/10, 1/4, 1/10, 1/10, 1/8])) ;;TODO check - cyan too frequent, possibile bug?
  ((alias-generator {"red" 1/5, "green" 1/2, "blue" 3/10})))

(defn weightings->probabilities [ws]
  (let [total (reduce + 0 ws)]
    (map #(/ % total) ws)))

(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #(<= % r) w)))))
