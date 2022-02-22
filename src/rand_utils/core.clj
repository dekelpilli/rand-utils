(ns rand-utils.core)

;https://www.keithschwarz.com/darts-dice-coins/
(defn alias-init [probabilities]
  "Performs the initialisation for Vose's Alias Method"
  (let [n (count probabilities)
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
        :else {:prob prob :alias alias}))))

(defn alias-gen [{:keys [prob alias]} values]
  (let [i (rand-int (count prob))
        heads? (<= (rand) (nth prob i))]
    (nth values (if heads? i (nth alias i)))))

(comment
  (-> (alias-init [2/10 5/10 3/10])
      (alias-gen ["red" "green" "blue"])))

(defn weightings->probabilities [ws]
  (let [total (reduce + 0 ws)]
    (map #(/ % total) ws)))

(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #(<= % r) w)))))
