(ns clox.interpreter
  (:require [clox.scanner :as scanner]
            [clojure.string :as str]
            [clox.errors :as errors]
            [clox.callable :as callable]
            [clox.environment :as env])
  (:import [clox.callable LoxCallable]))

(defn- trusy? [val]
  (cond
    (nil? val) false
    (boolean? val) val
    :else true))

(defn- equal?' [v1 v2]
  (cond
    (and (nil? v1) (nil? v2))
    true

    (or (nil? v1) (nil? v2))
    false

    :else (= v1 v2)))

(defn- check-number-operand [operator operand]
  (when-not (double? operand)
    (throw (errors/runtime-error operator "Operand must be a number."))))

(defn- check-number-operands [operator left right]
  (when-not (and (double? left) (double? right))
    (throw (errors/runtime-error operator "Operand must be a number."))))

(defn- set-result [ctx v]
  (assoc ctx ::result v))

(defn- get-result [ctx]
  (::result ctx))

(defn define-local-env! [ctx ename val]
  (env/define-env! (::env/local ctx) ename val)
  ctx)



(defn global-envirement []
  (let [env (env/create-env)]
    (env/define-env! env "clock"
      (reify callable/LoxCallable
        (call [this env _args]
          (set-result env
                      (double (/ (System/currentTimeMillis) 1000.0))))

        (arity [this] 0)
        (toString [this] "<native fn>")))
    {::env/local env
     ::env/global env}))


(defmulti evaluate (fn [_env {t :type}] t))
(defmulti execute (fn [_env {t :type}] t))

(defrecord Function [declaration env initializer?]
  LoxCallable
  (call [this call-env arguments]
    (let [params (get-in this [:declaration :params] )
          fn-env (->> (map vector params arguments)
                      (reduce (fn [env' [param arg]]
                                (define-local-env! env' (:lexeme param) arg))
                              (env/enclose-env (get-in this [:env]))))

          result (try (execute fn-env (get-in this [:declaration :body]))
                      (set-result call-env nil)
                      (if initializer?
                        (set-result call-env
                                    (env/get-env-at (get-in this [:env ::env/local]) {:lexeme "this"} 0))
                        (set-result call-env nil))
                      (catch clojure.lang.ExceptionInfo e
                        (when-not (= (:type (ex-data e))
                                     :fun-return)
                          (throw e))
                        (if initializer?
                          (set-result call-env
                                      (env/get-env-at (get-in this [:env ::env/local]) {:lexeme "this"} 0))
                          (set-result call-env (get-result (:env (ex-data e)))))))]

      result))

  (arity [this] (count (get-in this [:declaration :params] )))
  (toString [this] (str "<fn "  (get-in this [:declaration :name-token :lexeme]) ">")))

(defprotocol LoxInstance
  (getn [this token])
  (setn! [this token v]))

(defn- bind-this-env [method this]
  (let [cur-method-env (:env method)
        rebinded-env (env/enclose-env cur-method-env)]
    (define-local-env! rebinded-env "this" this)
    (assoc method :env rebinded-env)))

(defn find-method [{:keys [methods super-class] :as _class}
                   token]
  (if-let [method (get methods (:lexeme token))]
    method
    (when super-class
      (recur super-class token))))

(defrecord Instance
    [class* state]
  clox.interpreter.LoxInstance
  (getn [this token]
    (let [val (get-in @(:state this) [(:lexeme token)]
                      ::not-found)]
      (if (= ::not-found val)
        (if-let [method (find-method class* token)]
          (bind-this-env method this)
          (throw (errors/runtime-error token
                                       (str  "Undefined property '"
                                             (:lexeme token) "'."))))
        val)))
  (setn! [{state :state} token v]
    (swap! state assoc (:lexeme token) v))

  (toString [this] (str (get-in this [:class* :name]) " instance")))

(defrecord ClassDeclaration
  [name methods super-class]
  LoxCallable
  (call [this env arguments]
    (let [instance (->Instance this (atom {}))
          initializer (get methods "init")]
      (when initializer
        (callable/call (bind-this-env initializer instance)
                       env
                       arguments))
      (set-result env instance)))
  (arity [this] 0
    (if-let [initializer (get methods "init")]
      (callable/arity initializer)
      0))
  (toString [this] (:name this)))

(defmethod evaluate :expr/logical
  [env {:keys [left operator right]}]
  (let [env (evaluate env left)
        l-res (get-result env)]
    (if (trusy? l-res)
      (if (= (:type operator) ::scanner/or)
        env
        (evaluate env right))
      (if (= (:type operator) ::scanner/or)
        (evaluate env right)
        env))))

(defmethod evaluate :expr/binary
  [env {:keys [left operator right]}]
  (let [env (evaluate env left)
        l-res (get-result env)
        env (evaluate env right)
        r-res (get-result env)]
    (case (:type operator)
      ::scanner/plus
      (cond
        (and (number? l-res) (number? r-res))
        (set-result env (+ (double l-res) (double r-res)))

        (and (string? l-res) (string? r-res))
        (set-result env (str l-res r-res))
        :else (throw
               (errors/runtime-error operator "Operands must be two numbers or two strings.")))

      ::scanner/minus
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (- (double l-res) (double r-res))))

      ::scanner/star
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (* (double l-res) (double r-res))))

      ::scanner/slash
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (/ (double l-res) (double r-res))))

      ::scanner/greater
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (> (double l-res) (double r-res))))

      ::scanner/greater-equal
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (>= (double l-res) (double r-res))))

      ::scanner/less
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (< (double l-res) (double r-res))))

      ::scanner/less-equal
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (<= (double l-res) (double r-res))))

      ::scanner/equal-equal
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (equal?' (double l-res) (double r-res))))

      ::scanner/bang-equal
      (do
        (check-number-operands operator l-res r-res)
        (set-result env (not (equal?' (double l-res) (double r-res))))))))

(defmethod evaluate :expr/unary
  [env {:keys [operator right]}]
  (let [env (evaluate env right)
        res (get-result env)]
    (case (:type operator)
      ::scanner/minus
      (do
        (check-number-operand operator res)
        (set-result env (- (double res))))

      ::scanner/bang
      (set-result env (not (trusy? res))))))

(defmethod evaluate :expr/grouping
  [env {:keys [expression]}]
  (evaluate env expression))

(defmethod evaluate :expr/literal
  [env {:keys [value]}]
  (set-result env value))

(defn look-up-variable [ctx name-token]
  (if-let [depth (get-in ctx [:locals name-token])]
    (env/get-env-at (::env/local ctx) name-token depth)
    (env/get-env (::env/global ctx) name-token)))

(defn look-up-local-variable-at-offset [ctx from-token offset look-token]
  (let [depth (+ (get-in ctx [:locals from-token])
                 offset)]
    (env/get-env-at (::env/local ctx) look-token depth)))

(defmethod evaluate :expr/variable
  [ctx {:keys [name-token]}]
  (set-result ctx (look-up-variable ctx name-token)))

(defmethod evaluate :expr/this
  [ctx {:keys [token]}]
  (set-result ctx (look-up-variable ctx token)))

(defmethod evaluate :expr/super
  [ctx {:keys [token param]}]
  (let [super (look-up-variable ctx token)
        this (look-up-local-variable-at-offset ctx token -1 {:lexeme "this"})
        method (find-method super param)]
    (when-not method
      (throw
       (errors/runtime-error param
                             (str "Undefined property '" (:lexeme param) "'."))))
    (set-result ctx (bind-this-env method this))))

(defn- set-env-local! [ctx name-token val]
  (if-let [depth (get-in ctx [:locals name-token])]
    (env/set-env-at! (::env/local ctx) depth name-token val)
    (env/set-env! (::env/global ctx) name-token val))
  ctx)

(defmethod evaluate :expr/assign
  [env {:keys [name-token value]}]
  (let [env (evaluate env value)
        val-res (get-result env)
        env (set-env-local! env name-token val-res)]
    (set-result env val-res)))

(defmethod evaluate :expr/call
  [env {:keys [calle-expr paren-token arguments-exprs]}]
  (let [callee-ctx (evaluate env calle-expr)
        calle (get-result callee-ctx)
        {args :args :as env} (reduce (fn [env' arg]
                                       (let [env'' (evaluate env' arg)]
                                         (update env'' :args conj (get-result env''))))
                                     (assoc callee-ctx :args [])
                                     arguments-exprs)]
    (when-not (instance? LoxCallable calle)
      (throw (errors/runtime-error paren-token
                                   "Can only call functions and classes.")))
    (when (not= (callable/arity calle) (count args))
      (throw (errors/runtime-error paren-token
                                   (str "Expected "
                                        (callable/arity calle)  " arguments but got "
                                        (count args)  "."))))

    (callable/call calle env args)))


(defmethod evaluate :expr/get
  [env {:keys [object-expr name-token]}]
  (let [env* (evaluate env object-expr)
        inst (get-result env*)]
    (when-not (instance? clox.interpreter.LoxInstance inst)
      (throw (errors/runtime-error name-token
                                   "Only instances have properties.")))
    (set-result env* (getn inst name-token))))

(defmethod evaluate :expr/set
  [env {:keys [object-expr name-token value]}]
  (let [inst-env (evaluate env object-expr)
        inst (get-result inst-env)
        val-env (evaluate inst-env value)
        val (get-result val-env)]
    (when-not (instance? clox.interpreter.LoxInstance inst)
      (throw (errors/runtime-error name-token
                                   "Only instances have properties.")))
    (setn! inst name-token val)
    (set-result val-env #_inst val)))


(defn strinfigy [val]
  (cond
    (nil? val) "nil"

    (double? val)
    (if (str/ends-with? (str val) ".0")
      (subs (str val) 0 (- (count (str val)) 2))
      (str val))

    :else (str val)))


(defmethod execute
  :stmt/expression
  [env {:keys [expression]}]
  (evaluate env expression))

(defmethod execute
  :stmt/print
  [env {:keys [expression]}]
  (let [env (evaluate env expression)]
    (-> env
        get-result
        strinfigy
        println)
    env))

(defmethod execute :stmt/if
  [env {:keys [condition-expr then-stmt else-stmt]}]
  (let [env (evaluate env condition-expr)
        cond-val (-> env get-result trusy?)
        env (if cond-val
              (execute env then-stmt)
              (if else-stmt
                (execute env else-stmt)
                env))]
    env))

(defmethod execute :stmt/while
  [env' {:keys [condition-expr body]}]
  (loop [env env']
    (let [env (evaluate env condition-expr)
          cond-val (-> env get-result trusy?)]
      (if cond-val
        (recur (execute env body))
        env))))

(defmethod execute :stmt/var
  [env {:keys [name-token initializer]}]
  (let [env (if initializer
              (evaluate env initializer)
              env)
        res (when initializer (get-result env))]
    (define-local-env! env (:lexeme name-token) res)))

(defmethod execute :stmt/fun
  [env {:keys [name-token params body] :as declaration}]
  (define-local-env! env (:lexeme name-token) (->Function declaration env false)))

(defmethod execute :stmt/class
  [env {:keys [name-token super-class methods] :as declaration}]
  (let [super-class-instansce (when super-class
                                (look-up-variable env (:name-token super-class)))
        method-fns (reduce (fn [acc {:keys [name-token] :as declaration}]
                             (let [env (if super-class
                                         (-> env
                                             env/enclose-env
                                             (define-local-env! "super" super-class-instansce))
                                         env)]
                               (assoc acc (:lexeme name-token)
                                      (->Function declaration env
                                                  (if (= (:lexeme name-token) "init")
                                                    true
                                                    false)))))
                           {}
                           methods)]
    (when super-class
      (when-not (instance? clox.interpreter.ClassDeclaration super-class-instansce)
        (throw (errors/runtime-error name-token
                                     "Superclass must be a class."))))
    (define-local-env! env (:lexeme name-token)
      (->ClassDeclaration (:lexeme name-token)
                          method-fns
                          super-class-instansce))))


(defmethod execute :stmt/return
  [env {:keys [_keyword-token value-expr]}]
  (let [res (if value-expr
              (evaluate env value-expr)
              (set-result env nil))]
    (throw (ex-info "FUN RETURN" {:type :fun-return
                                  :env res}))))

(defmethod execute :stmt/block
  [env {:keys [statements]}]
  (env/denclose-env
   (reduce (fn [env-acc stmt]
             (execute env-acc stmt))
           (env/enclose-env env)
           statements)))


(defn interpret [stmts envirement locals]
  (reduce (fn [ctx stmt]
            (execute ctx stmt))
          (update (or envirement (global-envirement))
                  :locals merge locals)
          stmts))


(comment
  (defn- ev [s]
    (evaluate
     (:clox.parser/ast-node (clox.parser/parse (:tokens (clox.scanner/scanner s))))))

  (ev "5 != 4")

  (ev "\"4\" + 5")

  (/ 5 0.)


  )
