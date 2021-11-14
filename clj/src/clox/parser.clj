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
   :token     token
   :type :parser})

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

(defn- logic-creator [ctx base & operators]
  (loop [left (base ctx)]
    (if (apply match-token left operators)
      (let [operator (get-current-token left)
            right (base (advance left))
            expression (ast/new :expr/logical
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

    (match-token ctx ::scanner/true)
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

(defn- arguments [ctx]
  (loop [ctx ctx
         args []]
    (if (match-token ctx ::scanner/rparen)
      (set-ast-node ctx args)
      (let [{arg ::ast-node :as ctx} (expression ctx)
            args (conj args arg)]
        (if (match-token ctx ::scanner/comma)
          (recur (advance ctx) args)
          (set-ast-node ctx args))))))

(defn- call [ctx]
  (let [expr (primary ctx)]
    (loop [{calle-expr ::ast-node :as ctx} expr]
      (if (match-token ctx ::scanner/lparen)
        (let [lprn (get-current-token ctx)
              {arguments ::ast-node :as ctx} (arguments (advance ctx))]
          (recur (set-ast-node (consume-with-check ctx
                                                   ::scanner/rparen
                                                   "Expect ')' after function arguments")
                               (ast/new :expr/call
                                        calle-expr
                                        lprn
                                        arguments))))
        ctx))))

(defn- unary [ctx]
  (if (match-token ctx ::scanner/bang ::scanner/minus)
    (let [operator (get-current-token ctx)
          right (unary (advance ctx))]
      (set-ast-node right (ast/new :expr/unary
                                operator
                                (::ast-node right))))
    (call ctx)))

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

(defn- var-expression? [ast-node]
  (= :expr/variable (:type ast-node)))

(defn- logical-and [ctx]
  (logic-creator ctx equality
                  ::scanner/and))

(defn- logical-or [ctx]
  (logic-creator ctx logical-and
                  ::scanner/or))

(defn- assignment [ctx]
  (let [expr (logical-or ctx)]
    (if (match-token expr ::scanner/equal)
      (let [value (assignment (advance expr))]
        (if (var-expression? (::ast-node expr))
          (set-ast-node value
                        (ast/new :expr/assign
                                 (:name-token (::ast-node expr))
                                 (::ast-node value)))
          (throw-parser-error expr "Wrong assignment target")))
      expr)))

(defn- expression [ctx]
  (assignment ctx))

(defn- base-statement [ctx type]
  (let [expr (expression ctx)]
    (set-ast-node
     (consume-with-check expr
                         ::scanner/semicolon
                         "Expect ';' after expression.")
     (ast/new type (::ast-node expr)))))

(declare declaration)
(declare var-declaration)
(declare statement)

(defn- block [ctx']
  (loop [ctx ctx'
         statements []]
    (if (or (match-token ctx ::scanner/rbrace) (at-eof? ctx))
      (set-ast-node (consume-with-check ctx
                                        ::scanner/rbrace
                                        "Expect '}' after block.")
                    (ast/new :stmt/block
                             statements))
      (let [statement-ctx (declaration ctx)]
        (recur statement-ctx (conj statements (::ast-node statement-ctx)))))))

(defn- if-st [ctx]
  (let [ctx (consume-with-check ctx
                                ::scanner/lparen
                                "Expect '(' after if.")
        {expr ::ast-node :as ctx} (expression ctx)
        ctx (consume-with-check ctx
                                ::scanner/rparen
                                "Expect ')' after if condition.")
        {then ::ast-node :as ctx} (statement ctx)

        else-ctx (when (match-token ctx ::scanner/else)
                   (statement (advance ctx)))
        else (::ast-node else-ctx)
        ctx (or else-ctx ctx)]
    (set-ast-node ctx
                  (ast/new :stmt/if expr then else))))

(defn- while-st [ctx]
  (let [ctx (consume-with-check ctx
                                ::scanner/lparen
                                "Expect '(' after while.")
        {expr ::ast-node :as ctx} (expression ctx)
        ctx (consume-with-check ctx
                                ::scanner/rparen
                                "Expect ')' after while condition.")
        {body ::ast-node :as ctx} (statement ctx)]
    (set-ast-node ctx
                  (ast/new :stmt/while expr body))))

(defn- for-ast-b [initializer condition increment body]
  (let [body (if increment
               (ast/new :stmt/block
                        [body
                         (ast/new :stmt/expression increment)])
               body)

        body (ast/new :stmt/while
                      (or condition
                          (ast/new :expr/literal true))
                      body)

        body (if initializer
               (ast/new :stmt/block
                        [initializer
                         body])
               body)]
    body))

(defn- for-st [ctx]
  (let [ctx (consume-with-check ctx
                                ::scanner/lparen
                                "Expect '(' after 'for'.")
        {initializer ::ast-node :as ctx}
        (cond
          (match-token ctx ::scanner/semicolon)
          (assoc (advance ctx) ::ast-node nil)

          (match-token ctx ::scanner/var)
          (var-declaration (advance ctx))

          :else
          (base-statement ctx :stmt/expression))

        {condition ::ast-node :as ctx}
        (if (match-token ctx ::scanner/semicolon)
          (assoc ctx ::ast-node nil)
          (expression ctx))

        ctx (consume-with-check ctx
                                ::scanner/semicolon
                                "Expect ';' after loop condition.")

        {increment ::ast-node :as ctx}
        (if (match-token ctx ::scanner/rparen)
          (assoc ctx ::ast-node nil)
          (expression ctx))

        ctx (consume-with-check ctx
                                ::scanner/rparen
                                "Expect ')' after for clause.")

        {body ::ast-node :as ctx} (statement ctx)

        for-ast (for-ast-b initializer condition increment body)]
    (set-ast-node ctx for-ast)))

(defn- statement [ctx]
  (cond
    (match-token ctx ::scanner/print)
    (base-statement (advance ctx) :stmt/print)

    (match-token ctx ::scanner/lbrace)
    (block (advance ctx))

    (match-token ctx ::scanner/if)
    (if-st (advance ctx))

    (match-token ctx ::scanner/while)
    (while-st (advance ctx))

    (match-token ctx ::scanner/for)
    (for-st (advance ctx))

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

(defn- fun-declaration [ctx kind]
  (let [name (get-current-token ctx)
        ctx (consume-with-check ctx
                                ::scanner/identifier
                                (str "Expect " kind "name."))

        ctx (consume-with-check ctx
                                ::scanner/lparen
                                (str "Expect '(' after " kind "name."))
        {params ::ast-node :as ctx}
        (if (match-token ctx ::scanner/rparen)
          (set-ast-node ctx [])
          (loop [ctx ctx
                 params []]
            (let [params (conj params (get-current-token ctx))
                  ctx (consume-with-check ctx
                                          ::scanner/identifier
                                          "Expect parameter name.")]
              (if (match-token ctx ::scanner/comma)
                (recur (advance ctx) params)
                (set-ast-node ctx params)))))

        ctx (consume-with-check ctx
                                ::scanner/rparen
                                (str "Expect ')' after parameters"))

        ctx (consume-with-check ctx
                                ::scanner/lbrace
                                (str "Expect '{' before " kind "name."))

        {body ::ast-node :as ctx}
        (block ctx)]
    (set-ast-node ctx
                  (ast/new :stmt/fun
                           name
                           params
                           body))))

(defn- declaration [ctx]
  (cond
    (match-token ctx ::scanner/var)
    (var-declaration (advance ctx))

    (match-token ctx ::scanner/fun)
    (fun-declaration (advance ctx) "function")

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
      (ex-data e))))

(comment
  (parse (:tokens (clox.scanner/scanner "fun tmp (a, b) {print 5; print b;}")))

  (parse (:tokens (clox.scanner/scanner "help(1, 2)(777, 944);")))

  )
