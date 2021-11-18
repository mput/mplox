(ns clox.resolver)

;; TODO: add macros for defmethods (one decloration for multiple types)

(defn- new-parser-context []
  {::locals {}
   ::errors []
   ::scopes '()
   ::current-function ::none
   ::current-class ::none})


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

(defn- check-if-var-declared [ctx name-token]
  (if (get-in-top-scope ctx name-token)
    (add-error ctx name-token "Already a variable with this name in this scope.")
    ctx))

(defmethod resolve-ast :stmt/var
  [ctx {:keys [name-token initializer]}]
  (-> ctx
      (check-if-var-declared name-token)
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
  [ctx {:keys [_name-token params body]} type]
  (let [define-params (fn [ctx]
                        (reduce define* ctx params))
        in-cur-fun (::current-function ctx)]
    (-> ctx
        (assoc ::current-function type)
        (push-scope)
        (define-params)
        (resolve-ast body)
        (pop-scope)
        (assoc ::current-function in-cur-fun))))

(defmethod resolve-ast :stmt/fun
  [ctx {:keys [name-token _params _body] :as declaration}]
  (-> ctx
      (define* name-token)
      (resolve-function declaration ::function)))

{:type :stmt/class,
 :name-token
 {:type :clox.scanner/identifier,
  :lexeme "New",
  :line 1,
  :token-id "b31f6f60-ba7a-4657-be0d-633c288098cd"},
 :super-class
 {:type :expr/variable,
  :name-token
  {:type :clox.scanner/identifier,
   :lexeme "Old",
   :line 1,
   :token-id "a95bed26-2798-4bfb-80ca-306778878c01"}},
 :methods []}

(defmethod resolve-ast :stmt/class
  [ctx {:keys [name-token super-class methods]}]
  (let [in-cur-class (::current-class ctx)
        super-class-name (get-in super-class [:name-token :lexeme])
        class-name (get-in name-token [:lexeme])]
    (-> ctx
        (as-> ctx' (if (= super-class-name class-name)
                     (add-error ctx' name-token "A class can't inherit from itself.")
                     ctx'))
        (assoc ::current-class ::class)
        (define* name-token)
        (resolve-ast super-class)
        (push-scope)
        (define* {:lexeme "this"})
        (as-> ctx'
            (reduce (fn [ctx'' declaration]
                      (resolve-function ctx'' declaration
                                        (if (= (get-in declaration [:name-token :lexeme])
                                               "init")
                                          ::initializer
                                          ::method)))
                    ctx'
                    methods))
        (pop-scope)
        (assoc ::current-class in-cur-class))))

(defmethod resolve-ast :expr/this
  [ctx {:keys [token]}]
  (cond-> ctx
    (= (::current-class ctx)
       ::none)
    (add-error token "Can't use 'this' outside of a class.")

    :anyway (resolve-local token)))

(defmethod resolve-ast :stmt/expression
  [ctx {:keys [expression]}]
  (resolve-ast ctx expression))
(defmethod resolve-ast :stmt/print
  [ctx {:keys [expression]}]
  (resolve-ast ctx expression))
(defmethod resolve-ast :expr/grouping
  [ctx {:keys [expression]}]
  (resolve-ast ctx expression))

(defmethod resolve-ast :expr/get
  [ctx {:keys [object-expr]}]
  (resolve-ast ctx object-expr))

(defmethod resolve-ast :expr/set
  [ctx {:keys [object-expr _name-token value]}]
  (-> ctx
      (resolve-ast object-expr)
      (resolve-ast value)))

(defmethod resolve-ast :stmt/if
  [ctx {:keys [condition-expr then-stmt else-stmt]}]
  (-> ctx
      (resolve-ast condition-expr)
      (resolve-ast then-stmt)
      (resolve-ast else-stmt)))

(defmethod resolve-ast :stmt/return
  [ctx {:keys [keyword-token value-expr]}]
  (cond-> ctx
    (= (::current-function ctx)
       ::none)
    (add-error keyword-token "Can't return from top-level code.")

    (and (= (::current-function ctx)
            ::initializer)
         value-expr)
    (add-error keyword-token "Can't return from top-level code.")

    :anyway (resolve-ast value-expr)))

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
