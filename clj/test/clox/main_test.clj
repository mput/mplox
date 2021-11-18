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

  (run-intersept "print 5"
                 "[line 1] Error  : Expect ';' after expression.")

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

  (run-intersept
   "
var tmp = \"out\";
{
fun in() {
print tmp;
}
in();
var tmp = \"in\";
in();
}
"
   "out" "out"

   )

  (run-intersept
   "
{
 var tmp = \"out\";
 {
  fun in() {
  print tmp;
  }
  in();
  var tmp = \"in\";
  in();
 }
}
"
   "out" "out"

   )



  (run-intersept
   "var tmp = \"out\";
    {
    fun in() {
    tmp = \"redefine-out\";
    }
    var tmp = \"in\";
    in();
    }
    print tmp;"
   "redefine-out")

  (run-intersept
   "{var tmp = \"out\";
        {fun in() {
           tmp = \"redefine-out\";
         }
         var tmp = \"in\";
         in();
         }
         print tmp;
    }"
   "redefine-out")


  (run-intersept
   "var tmp = \"out\";
    {
    var tmp = \"in-\" + tmp;
    print tmp;
    }"
   "Can't read local variable in its own initializer. [line 3]")

  (run-intersept
   "{var a = 1;
     var a = 2;
    }"
   "Already a variable with this name in this scope. [line 2]")

  (run-intersept
   "return true;"
   "Can't return from top-level code. [line 1]")


  (run-intersept
   "
class Nice {
    beNice () {
    return \"You are Nice\";
    }
}
print Nice;
var nicer = Nice();
print nicer;
"
   "Nice"
   "Nice instance"
   )

  (run-intersept
   "
class Nice {
}
Nice().unk;
"
   "Undefined property 'unk'. [line 4]"
   )

  (run-intersept
   "
class Nice {
}
var nicer = Nice();
nicer.known = 4;
print nicer.known;
"
   4
   )

  (run-intersept
   "
class Nice {
sayHi (){
print \"Hi!\";
}
}
Nice().sayHi();
"
   "Hi!"
   )

(run-intersept
   "
class Nice {
sayHi (){
print \"Hi!\";
}
}
var nicer = Nice();
nicer.sayHi();
nicer.sayHi = 42;
nicer.sayHi();
"
   "Hi!"
   "Can only call functions and classes. [line 10]"
   )

  (run-intersept
   "
class Nice {
sayHi (name) {
return \"Hi \" + this.uname + \" \" + name + \"!\";
}
}
var nicer = Nice();
nicer.uname = \"Maxim\";
var maximGreater = nicer.sayHi;
print maximGreater(\"dear\");
nicer.uname = \"Nik\";
print maximGreater(\"my\");
"
   "Hi Maxim dear!"
   "Hi Nik my!"
   )

  (run-intersept
   "this;"
   "Can't use 'this' outside of a class. [line 1]")

  (run-intersept
    "
class User {
    init(nm) {
      this.name = nm;
    }
    names() {
      print this.name;
    }
}

var us = User(\"Maxim\");
us.names();
"
    "Maxim")


  (run-intersept
   "
class User {
  init() {
  }
  notinit() {
  }
}
var a = User();

print a.init();
print a.notinit();
"
"User instance"
"nil"
   )

(run-intersept
   "
class User {
  init() {
    return true;
  }
}"
   "Can't return from top-level code. [line 4]")

(run-intersept
   "
class User {
  init() {
    return;
  }
}
"
   "")

(run-intersept
   "
class Person {
}

class User < Person {
}
"
   "")


(run-intersept
   "
class User < User {
}
"
   "A class can't inherit from itself. [line 2]" )


(run-intersept
   "
var str = \"stirng\";
class User < str {
}
"
   "Superclass must be a class. [line 3]"
   )

;; "A class can't inherit from itself."
;; "Superclass must be a class."


  )
