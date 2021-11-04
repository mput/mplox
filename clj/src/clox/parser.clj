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

(defn- advance [ctx]
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

(defn create-error
  [token msg]
  {:message   msg
   :token    token})

(defn- throw-parser-error [ctx msg]
  (throw (ex-info "Parser error"
                  (-> ctx
                      (update ::errors  conj
                              (create-error (get-current-token ctx) msg))))))


(defn- consume-with-check [ctx token-type msg]
  (if (match-token ctx token-type)
    (advance ctx)
    (throw-parser-error ctx msg)))

(defn- binary-creator [ctx base & operators]
  (loop [left (base ctx)]
    (if (apply match-token left operators)
      (let [operator (get-current-token left)
            right (base (advance left))
            expression (expr/new ::expr/binary
                                 (::expr left)
                                 operator
                                 (::expr right))]
        (recur (set-expr right expression)))
      left)))

(declare expression)

(defn- primary [ctx]
  (cond
    (match-token ctx ::scanner/number ::scanner/string
                 ::scanner/true ::scanner/false ::scanner/nil)
    (advance (set-expr ctx (get-current-token ctx)))

    (match-token ctx ::scanner/lparen)
    (let [lprn (advance ctx)
          grp (expression lprn)
          exp (expr/new ::expr/grouping
                        (::expr grp))]
      (set-expr (consume-with-check grp
                                    ::scanner/rparen
                                    "Expect ')' after expression.")
                exp))

    :else (throw-parser-error ctx "Expect expression.")))

(defn- unary [ctx]
  (if (match-token ctx ::scanner/bang ::scanner/minus)
    (let [operator (get-current-token ctx)
          right (unary (advance ctx))]
      (set-expr right (expr/new ::expr/unary
                                operator
                                (::expr right))))
    (primary ctx)))

(defn- factor [ctx] (binary-creator ctx unary ::scanner/slash ::scanner/star))
(defn- term [ctx] (binary-creator ctx factor ::scanner/plus ::scanner/minus))
(defn- comprison [ctx]
  (binary-creator ctx term
                  ::scanner/greater
                  ::scanner/greater-equal
                  ::scanner/less
                  ::scanner/less-equal))

(defn- equality [ctx]
  (binary-creator ctx comprison
                  ::scanner/bang-equal ::scanner/equal-equal))

(defn- expression [ctx]
  (equality ctx))

(defn parse [tokens]
  (expression (new-parser-context tokens)))

(comment
  (parse (:tokens (clox.scanner/scanner " (2 + 3) * 4 == 4")))

  (parse (:tokens (clox.scanner/scanner " (2 + 4) == 4")))

  (::expr (parse (:tokens (clox.scanner/scanner "--2 "))))

  )
