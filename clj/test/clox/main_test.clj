(ns clox.main-test
  (:require [clox.main :as sut]
            [clojure.test :as t]
            [clojure.string :as str]))

(defn run-intersept [src exp-res]
  (t/is (= exp-res
         (str/trim (with-out-str (sut/run src :dont-exit))))))

(t/deftest main
  (run-intersept "print 5;" "5")

  (run-intersept "print 5 + 5 * 2;" "15")

  (run-intersept "print (5 + 5) * 2;" "20")


  )
