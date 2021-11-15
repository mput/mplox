(ns clox.main-test
  (:require [clox.main :as sut]
            [clojure.test :as t]
            [clojure.string :as str]))

(defn run-intersept [src & exp-res]
  (t/is (= (map str exp-res)
           (str/split (str/trim (with-out-str (sut/run src)))
                      #"\n"))))

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
                 "4" "2" "3" "0" "4")

  (run-intersept "if (true) print 5;"
                 "5")

  (run-intersept "if (false) print 5;
                  else print 6;"
                 "6")

  (run-intersept "var a = 1;
                  if (false) {print 5;}
                  else print 6;
                  print a;"
                 "6" "1")


  (run-intersept "var a = false or 5;
print a;" "5")

  (run-intersept "var a = 5 or false;
print a;" "5")


  (run-intersept "var a = false or nil;
print a;" "nil")

  (run-intersept "var a = 5 and false;
print a;" "false")

  (run-intersept "var a = 5 and 6;
print a;" "6")


  (run-intersept
   "var a = 3;
    while (a > 1) {
    print a;
    a = a - 1;
    }
    print 10 * a;"
   3 2 10)

  (run-intersept
   "for (var a = 3 ; a>1; a = a - 1 ) print a;
    print a;"
   3 2 "Undefined variable 'a'. [line 2]")


  (run-intersept
   "
var a = 0;
var temp;

for (var b = 1; a < 50; b = temp + b) {
  print a;
  temp = a;
  a = b;
}
"
   "0" "1" "1" "2" "3" "5" "8" "13" "21" "34")

  (run-intersept "
fun sayHi(first, last) {
  print \"Hi, \" + first + \" \" + last + \"!\";
}
sayHi(\"Dear\", \"Reader\");
"
                 "Hi, Dear Reader!")

  (run-intersept "
fun toTen(a) {
return a * 10;
}
print  toTen(42);
"
                 420)


  (run-intersept "
fun toTen(a) {
return;
}
print  toTen(42);
"
                 "nil")


  (run-intersept "
fun makeCounter (init) {
var cnt = init;
fun count() {
print init = init + 1;
}
return count;
}

var cnt1 = makeCounter(10);
var cnt2 = makeCounter(42);

cnt1();
cnt2();
cnt1();
cnt2();
"
                 11 43 12 44
                 )



  )
