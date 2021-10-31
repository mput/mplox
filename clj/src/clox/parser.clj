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


(defn- get-current [{::keys [tokens current]}]
  (nth tokens current))

(defn- consume [ctx]
  (update ctx ::current inc))

(defn- set-expr [ctx expr]
  (assoc ctx ::expr expr))

(defn- get-expr [ctx expr]
  (::expr ctx))


(defn- match [ctx & token-types]
  (loop [[token & rest] token-types]
    (if token
      (or (= (:type (get-current ctx))
             token)
          (recur rest))
      false)))

(defn- primary [ctx]
  (let [val (get-current ctx)]
    (consume (set-expr ctx val))
    )
  )

(defn- factor [ctx]
  (primary ctx))

(defn- term [ctx']
  (loop [{left-expr ::expr :as ctx} (factor ctx')]
    (if (match ctx ::scanner/plus ::scanner/minus)
      (recur (let [operator (get-current ctx)

                   {right-expr ::expr :as ctx'}
                   (factor (consume ctx))

                   expression (expr/new ::expr/binary
                                        left-expr
                                        operator
                                        right-expr)]
               (set-expr ctx' expression)))
      ctx)))

;; (defn- equality [ctx]
;;   {})

(defn- expression [ctx]
  #_(equality ctx)
  (term ctx))

(defn parse [tokens]
  (expression (new-parser-context tokens))
  )


(comment
  (::expr (parse (:tokens (clox.scanner/scanner "2 - 3 + 4"))))


  )
