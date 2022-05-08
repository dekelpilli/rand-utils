(ns build
  "
  clojure -T:build ci

  clojure -T:build run-doc-tests :aliases '[:cljs]'

  Run tests:
  clojure -X:test
  clojure -X:test:master

  For more information, run:

  clojure -A:deps -T:build help/doc
  "
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb]))

(def lib 'com.github.dekelpilli/randy)
(def version (format "0.0.%s" (b/git-count-revs nil)))

(defn test [opts]
  (bb/run-tests opts))

(defn clean [opts]
  (bb/clean opts))

(defn jar [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/jar)))

(defn deploy [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/deploy)))
