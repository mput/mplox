(ns clox.parser
  (:require [clox.scanner :as scanner]
            [clox.expression :as expr]))

;; expression     → equality ;
;; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
;; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
;; term           → factor ( ( "-" | "+" ) factor )* ;
;; factor         → unary ( ( "/" | "*" ) unary )* ;
;; unary          → ( "!" | "-" ) unary
;;                | primary ;
;; primary        → NUMBER | STRING | "true" | "false" | "nil"
;;                | "(" expression ")" ;


(defn- new-parser-context [tokens]
  {::tokens tokens
   ::expr nil
   ::current 0
   ::errors []})

(defn- get-current-token [{::keys [tokens current]}]
  (nth tokens current))

(defn- consume-token [ctx]
  (update ctx ::current inc))

(defn- set-expr [ctx expr]
  (assoc ctx ::expr expr))

(defn- match-token [ctx & token-types]
  (loop [[token & rest] token-types]
    (if token
      (or (= (:type (get-current-token ctx))
             token)
          (recur rest))
      false)))

(defn- binary-creator [ctx base & operators]
  (loop [left (base ctx)]
    (if (apply match-token left operators)
      (let [operator (get-current-token left)
            right (base (consume-token left))
            expression (expr/new ::expr/binary
                                 (::expr left)
                                 operator
                                 (::expr right))]
        (recur (set-expr right expression)))
      left)))

(defn- primary [ctx]
  (let [val (get-current-token ctx)]
    (consume-token (set-expr ctx val))))

(defn- factor [ctx] (binary-creator ctx primary ::scanner/slash ::scanner/star))
(defn- term [ctx] (binary-creator ctx factor ::scanner/plus ::scanner/minus))
(defn- comprison [ctx]
  (binary-creator ctx term
                  ::scanner/greater
                  ::scanner/greater-equal
                  ::scanner/less
                  ::scanner/less-equal))

(defn- equality [ctx]
  (binary-creator ctx comprison
                  ::scanner/bang-equal ::scanner/equal))

(defn- expression [ctx]
  (equality ctx))

(defn parse [tokens]
  (expression (new-parser-context tokens)))

(comment
  (::expr (parse (:tokens (clox.scanner/scanner " 2 + 3 + 4 = 4"))))


  )
