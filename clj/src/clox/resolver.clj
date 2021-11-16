(ns clox.resolver)

(defn- new-parser-context []
  {::locals {}
   ::errors []
   ::scopes '()})


(defn- push-scope [ctx]
  (update ctx ::scopes conj {}))

(defn- pop-scope [ctx]
  (update ctx ::scopes rest))

(defn- empty-scope? [{scopes ::scopes}]
  (empty? scopes))

(defn get-in-top-scope [{[top & _rest] ::scopes} {name :lexeme}]
  (get top name))

(defn assoc-first-if-present [[first & rest] k v]
  (if first
    (conj rest (assoc first k v))
    '()))

(defn add-locals [ctx token depth]
  (assoc-in ctx [::locals token] depth))

(defn create-error [token msg]
  {:type :resolver-error
   :token token
   :message msg})

(defn add-error [ctx token error-msg]
  (update ctx ::errors conj (create-error token error-msg)))

(defn- declare*
  [ctx name-token]
  (update ctx ::scopes
          assoc-first-if-present (:lexeme name-token) ::declared))

(defn- define*
  [ctx name-token]
  (update ctx ::scopes
          assoc-first-if-present (:lexeme name-token) ::defined))

(defn- resolved-at [ctx name-token]
  (loop [[top & rest] (::scopes ctx)
         depth 0]
    (when top
      (if (get top (:lexeme name-token))
        depth
        (recur rest (inc depth))))))

(defn- resolve-local [ctx name-token]
  (let [depth (resolved-at ctx name-token)]
    (if depth
      (add-locals ctx name-token depth)
      ctx)))


(defmulti resolve-ast (fn [_ctx {t :type :as expr}]
                        (if expr
                          t
                          ::no-expr)))

(defn resolve-all [ctx statements]
  (reduce resolve-ast ctx statements))

(defmethod resolve-ast ::no-expr
  [ctx _nil-here]
  ctx)

(defmethod resolve-ast :stmt/block
  [ctx {:keys [statements]}]
  (-> ctx
      (push-scope)
      (resolve-all statements)
      (pop-scope)))

(defmethod resolve-ast :stmt/var
  [ctx {:keys [name-token initializer]}]
  (-> ctx
      (declare* name-token)
      (resolve-ast initializer)
      (define* name-token)))

(defmethod resolve-ast :expr/variable
  [ctx {:keys [name-token]}]
  (if (and (not (empty-scope? ctx))
           (= (get-in-top-scope ctx name-token)
              ::declared))
    (add-error ctx name-token "Can't read local variable in its own initializer.")
    (resolve-local ctx name-token)))

(defmethod resolve-ast :expr/assign
  [ctx {:keys [name-token value]}]
  (-> ctx
      (resolve-ast value)
      (resolve-local name-token)))

(defn- resolve-function
  [ctx {:keys [_name-token params body]}]
  (letfn [(define-params [ctx]
            (reduce define* ctx params))]
    (-> ctx
        (push-scope)
        (define-params)
        (resolve-ast body)
        (pop-scope))))

(defmethod resolve-ast :stmt/fun
  [ctx {:keys [name-token _params _body] :as declaration}]
  (-> ctx
      (define* name-token)
      (resolve-function declaration)))

(defmethod resolve-ast :stmt/expression
  [ctx {:keys [expression]}]
  (resolve-ast ctx expression))
(defmethod resolve-ast :stmt/print
  [ctx {:keys [expression]}]
  (resolve-ast ctx expression))
(defmethod resolve-ast :expr/grouping
  [ctx {:keys [expression]}]
  (resolve-ast ctx expression))

(defmethod resolve-ast :stmt/if
  [ctx {:keys [condition-expr then-stmt else-stmt]}]
  (-> ctx
      (resolve-ast condition-expr)
      (resolve-ast then-stmt)
      (resolve-ast else-stmt)))

(defmethod resolve-ast :stmt/return
  [ctx {:keys [_keyword-token value-expr]}]
  (resolve-ast ctx value-expr))

(defmethod resolve-ast :stmt/while
  [ctx {:keys [condition-expr body]}]
  (-> ctx
      (resolve-ast condition-expr)
      (resolve-ast body)))

(defmethod resolve-ast :expr/binary
  [ctx {:keys [left right]}]
  (-> ctx
      (resolve-ast left)
      (resolve-ast right)))

(defmethod resolve-ast :expr/logical
  [ctx {:keys [left right]}]
  (-> ctx
      (resolve-ast left)
      (resolve-ast right)))

(defmethod resolve-ast :expr/unary
  [ctx {:keys [operator right]}]
  (resolve-ast ctx right))

(defmethod resolve-ast :expr/call
  [ctx {:keys [calle-expr arguments-exprs]}]
  (-> ctx
      (resolve-ast calle-expr)
      (resolve-all arguments-exprs)))

(defmethod resolve-ast :expr/literal
  [ctx {:keys [_value]}]
  ctx)

(defn resolver [stmts]
  (resolve-all (new-parser-context)
               stmts)

  )



(comment
  (defn- ev [s]
    (resolver
     (:clox.parser/statements (clox.parser/parse (:tokens (clox.scanner/scanner s)))))
    #_(:clox.parser/statements (clox.parser/parse (:tokens (clox.scanner/scanner s))))
    )


  (ev "
{
{
var f;
fun tt(a) {
var f = 2 +f + 1;
a = 0;
b;
}
{f;}
}
}
")


  )
