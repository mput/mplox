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

  (run-intersept "var a = 10;
                  var b;
                  a + b = 13;"
                 "[line 3] Error = : Wrong assignment target")

  (run-intersept "a = 13;"
                 "Undefined variable 'a'. [line 1]")

  (run-intersept "print a;"
                 "Undefined variable 'a'. [line 1]")

  (run-intersept "var a = 1;
                  var b = 2;
                  var z = 0;
                  {a = 4;
                   var z = 3;
                   print a;
                   print b;
                   print z;
                  }
                  print z;
                  print a;
                  "
                 "4\n2\n3\n0\n4")

  (run-intersept "if (true) print 5;"
                 "5")

  (run-intersept "if (false) print 5;
                else print 6;"
                 "5")

  (run-intersept "var a = 1;
                  if (false) {print 5;}
                  else print 6;
                  print a;"
                 "5\n1")

  )
