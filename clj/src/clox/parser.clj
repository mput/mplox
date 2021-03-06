(ns clox.parser
  (:require [clox.scanner :as scanner]
            [clox.ast :as ast]))

(defn- new-parser-context [tokens]
  {::tokens tokens
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



(defn- +token-param [ctx]
  (-> ctx
      (update ::node-params
              (fnil conj [])
              (get-current-token ctx))
      (advance)))

(defn- +consume-with-check [ctx token-type msg]
  (if (match-token ctx token-type)
    (+token-param ctx)
    (throw-parser-error ctx msg)))

(defn- +skip-param [ctx]
  (-> ctx
      (update ::node-params (fnil conj []) nil)))

(defn- +arbitrary-param [ctx val]
  (-> ctx
      (update ::node-params (fnil conj []) val)))

(defn- +node-param [ctx node-parser]
  (let [store-params (::node-params ctx)
        after-parse-ctx (-> ctx
                            (dissoc ::node-params)
                            (node-parser)
                            (assoc ::node-params store-params))]
    (-> after-parse-ctx
        (update ::node-params
                (fnil conj [])
                (::ast-node after-parse-ctx))
        (dissoc ::ast-node))))

(defn- +node-param-conj-last [ctx node-parser]
  (let [store-params (::node-params ctx)
        after-parse-ctx (-> ctx
                            (dissoc ::node-params)
                            (node-parser)
                            (assoc ::node-params store-params))

        new-params (conj (vec (butlast store-params))
                         (conj (last store-params)
                               (::ast-node after-parse-ctx)))]
    (-> after-parse-ctx
        (assoc ::node-params new-params)
        (dissoc ::ast-node))))


(defn- set-node [ctx type]
  (let [node (apply ast/new (cons type (::node-params ctx)))]
    (-> ctx
        (assoc ::ast-node node)
        (dissoc ::node-params))))


(defn- binary-creator [ctx type base operators]
  (loop [left-ctx (base ctx)]
    (if (apply match-token left-ctx operators)
      (recur (-> left-ctx
                 (+arbitrary-param (::ast-node left-ctx))
                 (+token-param)
                 (+node-param base)
                 (set-node type)))
      left-ctx)))


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

    (match-token ctx ::scanner/this)
    (advance (set-ast-node ctx (ast/new :expr/this (get-current-token ctx))))

    (match-token ctx ::scanner/super)
    (-> ctx
        (+token-param)
        (consume-with-check ::scanner/dot "Expect '.' after 'super'.")
        (+consume-with-check ::scanner/identifier "Expect superclass method name.")
        (set-node :expr/super))

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
  (loop [{calle-expr ::ast-node :as ctx} (primary ctx)]
    (cond
      (match-token ctx ::scanner/lparen)
      (let [next-call-ctx
            (-> ctx
                (+arbitrary-param calle-expr)
                (+token-param)
                (+node-param arguments)
                (consume-with-check ::scanner/rparen
                                    "Expect ')' after function arguments")
                (set-node :expr/call))]
        (recur next-call-ctx))

      (match-token ctx ::scanner/dot)
      (let [next-call-ctx
            (-> ctx
                advance
                (+arbitrary-param calle-expr)
                (+consume-with-check ::scanner/identifier
                                     "Expect property name after '.'.")
                (set-node :expr/get))]
        (recur next-call-ctx))

      :else ctx)))

(defn- unary [ctx]
  (if (match-token ctx ::scanner/bang ::scanner/minus)
    (-> ctx
        (+token-param)
        (+node-param unary)
        (set-node :expr/unary))
    (call ctx)))

(defn- factor [ctx] (binary-creator ctx :expr/binary unary
                                    [::scanner/slash ::scanner/star]))
(defn- term [ctx] (binary-creator ctx :expr/binary factor
                                  [::scanner/plus ::scanner/minus]))

(defn- comprison [ctx]
  (binary-creator ctx :expr/binary term
                  [::scanner/greater
                   ::scanner/greater-equal
                   ::scanner/less
                   ::scanner/less-equal]))

(defn- equality [ctx]
  (binary-creator ctx :expr/binary comprison
                  [::scanner/bang-equal ::scanner/equal-equal]))


(defn- logical-and [ctx]
  (binary-creator ctx :expr/logical equality
                 [::scanner/and]))
(defn- logical-or [ctx]
  (binary-creator ctx :expr/logical logical-and
                 [::scanner/or]))

(defn- var-expression? [ast-node]
  (= :expr/variable (:type ast-node)))

(defn- get-expression? [ast-node]
  (= :expr/get (:type ast-node)))

(defn- assignment [ctx]
  (let [expr (logical-or ctx)]
    (if (match-token expr ::scanner/equal)
      (let [value (assignment (advance expr))]
        (cond
          (var-expression? (::ast-node expr))
          (set-ast-node value
                        (ast/new :expr/assign
                                 (:name-token (::ast-node expr))
                                 (::ast-node value)))

          (get-expression? (::ast-node expr))
          (set-ast-node value
                        (ast/new :expr/set
                                 (:object-expr (::ast-node expr))
                                 (:name-token (::ast-node expr))
                                 (::ast-node value)))

          :else (throw-parser-error expr "Wrong assignment target")))
      expr)))

(defn- expression [ctx]
  (assignment ctx))

(defn- base-statement [ctx type]
  (-> ctx
      (+node-param expression)
      (consume-with-check ::scanner/semicolon
                          "Expect ';' after expression.")
      (set-node type)))

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
  (-> ctx
      (consume-with-check ::scanner/lparen
                          "Expect '(' after if.")
      (+node-param expression)
      (consume-with-check ::scanner/rparen
                          "Expect ')' after if condition.")
      (+node-param statement)
      (as-> ctx' (if (match-token ctx' ::scanner/else)
                   (+node-param (advance ctx') statement)
                   (+skip-param ctx')))
      (set-node :stmt/if)))

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

(defn- return [ctx]
  (let [keyword (get-current-token ctx)
        ctx (advance ctx)
        {value ::ast-node :as ctx}
        (if (match-token ctx ::scanner/semicolon)
          (assoc ctx ::ast-node nil)
          (expression ctx))]
    (set-ast-node (consume-with-check ctx
                                      ::scanner/semicolon
                                      "Expect ';' after return statement.")
                  (ast/new :stmt/return keyword value))))

(defn- statement [ctx]
  (cond
    (match-token ctx ::scanner/print)
    (base-statement (advance ctx) :stmt/print)

    (match-token ctx ::scanner/return)
    (return ctx)

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

(defn- fun-declaration [kind ctx]
  (let [name (get-current-token ctx)
        ctx (consume-with-check ctx
                                ::scanner/identifier
                                (str "Expect " kind "name."))

        ctx (consume-with-check ctx
                                ::scanner/lparen
                                (str "Expect '(' after " kind " name."))
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
                                (str "Expect '{' before " kind " name."))

        {body ::ast-node :as ctx}
        (block ctx)]
    (set-ast-node ctx
                  (ast/new :stmt/fun
                           name
                           params
                           body))))

(defn- class-declaration [ctx]
  (let [ctx (-> ctx
                (+consume-with-check ::scanner/identifier "Expect class name.")
                (as-> ctx' (if (match-token ctx' ::scanner/less)
                             (let [ctx'' (advance ctx')
                                   var (ast/new :expr/variable
                                                (get-current-token ctx''))]
                               (-> ctx''
                                   (consume-with-check ::scanner/identifier "Expect superclass name.")
                                   (+arbitrary-param var)))
                             (-> ctx'
                                 (+arbitrary-param nil))))
                (consume-with-check ::scanner/lbrace "Expect '{' before class body."))
        methods-ctx (loop [ctx' (+arbitrary-param ctx [])]
                      (if (or (match-token ctx' ::scanner/rbrace)
                              (at-eof? ctx'))
                        ctx'
                        (recur (+node-param-conj-last ctx'
                                                      (partial fun-declaration "method")))))]
    (-> methods-ctx
        (consume-with-check ::scanner/rbrace "Expect '}' before class body.")
        (set-node :stmt/class))))

(defn- declaration [ctx]
  (cond
    (match-token ctx ::scanner/var)
    (var-declaration (advance ctx))

    (match-token ctx ::scanner/fun)
    (fun-declaration "function" (advance ctx))


    (match-token ctx ::scanner/class)
    (class-declaration (advance ctx))

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
  (parse (:tokens (clox.scanner/scanner "this;")))

  (parse (:tokens (clox.scanner/scanner "help(1, 2)(777, 944);")))


  (parse (:tokens (clox.scanner/scanner "class New < Old{}")))

  )
