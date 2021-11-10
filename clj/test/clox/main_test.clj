(ns clox.main-test
  (:require [clox.main :as sut]
            [clojure.test :as t]
            [clojure.string :as str]))

(defn run-intersept [src exp-res & [exec]]
  (if exec
    (do
      (sut/run src))
    (t/is (= exp-res
         (str/trim (with-out-str (sut/run src)))))))

(t/deftest main
  (run-intersept "print 5;" "5")

  (run-intersept "print (-5);" "-5")

  (run-intersept "print (!!5);" "true")


  (run-intersept "var tvar = 3 * 2;
                  var bvar = 100;
                  print tvar * (-bvar);"
                 "-600")

  (run-intersept "print 5 + 5 * 2;" "15")

  (run-intersept "print (5 + 5) * 2;" "20")

  (run-intersept "var tvar;
                  tvar = 50;
                  print tvar;"
                 "50")

  (run-intersept "var a = 10;
                  var b;
                  a = b = 13;
                  print a + b;"
                 "26")

  ;; (run-intersept "var a = 10;
  ;;                 var b;
  ;;                 a + b = 13;"
  ;;                nil :true)

  (t/is (=
         (run-intersept "a = 13;"
                        nil :t)
         :clox.main/runtime-error))

  (t/is (=
         (run-intersept "print a;"
                        nil :t)
         :clox.main/runtime-error))



  )
