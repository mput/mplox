(ns clox.parser
  (:require [clox.scanner :as scanner]
            [clox.ast :as ast]))

(defn- new-parser-context [tokens]
  {::tokens tokens
   ::ast-node nil
   ::current 0
   ::statements []
   ::errors []})

(defn- get-current-token [{::keys [tokens current]}]
  (nth tokens current))


(defn- advance [ctx]
  (update ctx ::current inc))

(defn- set-ast-node [ctx expr]
  (assoc ctx ::ast-node expr))

(defn- match-token [ctx & token-types]
  (loop [[token & rest] token-types]
    (if token
      (or (= (:type (get-current-token ctx))
             token)
          (recur rest))
      false)))

(defn- at-eof? [ctx]
  (match-token ctx ::scanner/eof))

(defn create-error
  [token msg]
  {:message   msg
   :token     token})

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
            expression (ast/new :expr/binary
                                 (::ast-node left)
                                 operator
                                 (::ast-node right))]
        (recur (set-ast-node right expression)))
      left)))

(declare expression)

(defn- primary [ctx]
  (cond
    (match-token ctx ::scanner/number ::scanner/string)
    (advance (set-ast-node ctx (ast/new :expr/literal
                                     (:literal (get-current-token ctx)))))

    (match-token ctx ::scanner/true ::scanner/false ::scanner/nil)
    (advance (set-ast-node ctx (ast/new :expr/literal true)))

    (match-token ctx ::scanner/false)
    (advance (set-ast-node ctx (ast/new :expr/literal false)))

    (match-token ctx ::scanner/nil)
    (advance (set-ast-node ctx (ast/new :expr/literal nil)))

    (match-token ctx ::scanner/lparen)
    (let [lprn (advance ctx)
          grp (expression lprn)
          exp (ast/new :expr/grouping
                        (::ast-node grp))]
      (set-ast-node (consume-with-check grp
                                    ::scanner/rparen
                                    "Expect ')' after expression.")
                exp))

    (match-token ctx ::scanner/identifier)
    (advance (set-ast-node ctx (ast/new :expr/variable
                                        (get-current-token ctx))))

    :else (throw-parser-error ctx "Expect expression.")))

(defn- unary [ctx]
  (if (match-token ctx ::scanner/bang ::scanner/minus)
    (let [operator (get-current-token ctx)
          right (unary (advance ctx))]
      (set-ast-node right (ast/new :expr/unary
                                operator
                                (::ast-node right))))
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

(defn- base-statement [ctx type]
  (let [expr (expression ctx)]
    (set-ast-node
     (consume-with-check expr
                         ::scanner/semicolon
                         "Expect ';' after expression.")
     (ast/new type (::ast-node expr)))))

(defn- statement [ctx]
  (cond
    (match-token ctx ::scanner/print)
    (base-statement (advance ctx) :stmt/print)

    :else
    (base-statement ctx :stmt/expression)))

(defn- var-declaration [ctx]
  (let [identifier-ctx (consume-with-check ctx
                                           ::scanner/identifier
                                           "Expect 'identifier' after var.")
        expression-ctx (when (match-token identifier-ctx ::scanner/equal)
                         (expression (advance identifier-ctx)))
        final-ctx (or expression-ctx identifier-ctx)]
    (set-ast-node
     (consume-with-check final-ctx
                         ::scanner/semicolon
                         "Expect ';' after statement.")
     (ast/new :stmt/var
              (get-current-token ctx)
              (::ast-node expression-ctx)))))

(defn- declaration [ctx]
  (cond
    (match-token ctx ::scanner/var)
    (var-declaration (advance ctx))

    :else (statement ctx)))

(defn parse [tokens]
  (try
    (loop [ctx (new-parser-context tokens)]
      (if (at-eof? ctx)
        ctx
        (recur (let [{statement ::ast-node :as res} (declaration ctx)]
                 (-> res
                     (update ::statements conj statement))))))
    (catch clojure.lang.ExceptionInfo e
      (println (ex-message e))
      (println (ex-data e)))))

(comment
  (parse (:tokens (clox.scanner/scanner " (2 + 3) * 4 == 4")))

  (parse (:tokens (clox.scanner/scanner "help;")))

  (parse (:tokens (clox.scanner/scanner "var help = 4;")))


  (parse (:tokens (clox.scanner/scanner "print 5; (2 + 4) == 4;")))

  (parse (:tokens (clox.scanner/scanner "!true")))

  )
