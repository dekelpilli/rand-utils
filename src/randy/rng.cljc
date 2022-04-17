(ns randy.rng
  #?(:clj (:import (java.util Random))))

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
