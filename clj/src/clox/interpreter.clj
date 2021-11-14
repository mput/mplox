(ns clox.interpreter
  (:require [clox.scanner :as scanner]
            [clojure.string :as str]
            [clox.errors :as errors]
            [clox.callable :as callable])
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

(defn- define-env [ctx ename val]
  (assoc-in ctx [::values ename] val))

(defn- get-env [ctx name-token]
  (let [v (get-in ctx [::values (:lexeme name-token)] ::not-found)]
    (if (= v ::not-found)
      (if-let [parent (::parent ctx)]
        (get-env parent name-token)
        (throw (errors/runtime-error name-token
                                     (str "Undefined variable '"
                                          (:lexeme name-token)
                                          "'."))))
      v)))

(defn- set-env [ctx name-token val]
  (if (nil? ctx)
    (throw (errors/runtime-error name-token
                                 (str "Undefined variable '"
                                      (:lexeme name-token)
                                      "'.")))
    (let [v (get-in ctx [::values (:lexeme name-token)] ::not-found)]
      (if (= v ::not-found)
        (assoc ctx ::parent
               (set-env (::parent ctx) name-token val))
        (assoc-in ctx [::values (:lexeme name-token)] val)))))

(defn- enclose-env [env]
  {::parent env})

(defn- denclose-env [env]
  (::parent env))


(defmulti evaluate (fn [_env {t :type}] t))
(defmulti execute (fn [_env {t :type}] t))

(defrecord Function [declaration]
  LoxCallable
  (call [this env arguments]
    (let [params (get-in this [:declaration :params] )
          fn-env (->> (map vector params arguments)
                      (reduce (fn [env' [param arg]]
                                (define-env env' (:lexeme param) arg)) env))

          result (try (execute fn-env (get-in this [:declaration :body]))
                      (set-result env nil)
                      (catch clojure.lang.ExceptionInfo e
                        (when-not (= (:type (ex-data e))
                                     :fun-return)
                          (throw e))
                        (set-result env (get-result (:env (ex-data e))))))]

      result))

  (arity [this] (count (get-in this [:declaration :params] )))
  (toString [this] (str "<fn "  (get-in this [:declaration :name-token :lexeme]) ">")))

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


(defmethod evaluate :expr/variable
  [env {:keys [name-token]}]
  (set-result env (get-env env name-token)))

(defmethod evaluate :expr/assign
  [env {:keys [name-token value]}]
  (let [env (evaluate env value)
        val-res (get-result env)
        env (set-env env name-token val-res)]
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

(defmethod execute
  :stmt/var
  [env {:keys [name-token initializer]}]
  (let [env (if initializer
              (evaluate env initializer)
              env)
        res (when initializer (get-result env))]
    (define-env env (:lexeme name-token) res)))

(defmethod execute :stmt/fun
  [env {:keys [name-token params body] :as declaration}]
  (define-env env (:lexeme name-token) (->Function declaration)))

(defmethod execute :stmt/return
  [env {:keys [keyword-token value-expr]}]
  (let [res (if value-expr
              (evaluate env value-expr)
              (set-result env nil))]
    (throw (ex-info "FUN RETURN" {:type :fun-return
                                  :env res}))))

(defmethod execute :stmt/block
  [env {:keys [statements]}]
  (denclose-env
   (reduce (fn [env-acc stmt]
             (execute env-acc stmt))
           (enclose-env env)
           statements)))

(def global-envirement
  (-> {}
      (define-env "clock"
        (reify callable/LoxCallable
          (call [this env _args]
            (set-result env
                        (double (/ (System/currentTimeMillis) 1000.0))))

          (arity [this] 0)
          (toString [this] "<native fn>")))))


(defn interpret [stmts envirement]
  (reduce (fn [ctx stmt]
            (execute ctx stmt))
          (or envirement global-envirement)
          stmts))


(comment
  (defn- ev [s]
    (evaluate
     (:clox.parser/ast-node (clox.parser/parse (:tokens (clox.scanner/scanner s))))))

  (ev "5 != 4")

  (ev "\"4\" + 5")

  (/ 5 0.)


  )
