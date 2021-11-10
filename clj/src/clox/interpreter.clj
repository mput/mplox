(ns clox.interpreter
  (:require [clox.scanner :as scanner]
            [clojure.string :as str]
            [clox.errors :as errors]))

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


(defn- define-env [ctx name-token val]
  (assoc-in ctx [::values (:lexeme name-token)] val))

(defn- get-env [ctx name-token]
  (let [v (get-in ctx [::values (:lexeme name-token)] ::not-found)]
    (when (= v ::not-found)
      (throw (errors/runtime-error name-token
                                   (str "Undefined variable '"
                                        (:lexeme name-token)
                                        "'."))))
    v))

(defn- set-env [ctx name-token val]
  (get-env ctx name-token)
  (assoc-in ctx [::values (:lexeme name-token)] val))


(defmulti evaluate (fn [_env {t :type}] t))

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

(defn strinfigy [val]
  (cond
    (nil? val) "null"

    (double? val)
    (if (str/ends-with? (str val) ".0")
      (subs (str val) 0 (- (count (str val)) 2))
      (str val))

    :else (str val)))

(defmulti execute (fn [_env {t :type}] t))

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

(defmethod execute
  :stmt/var
  [env {:keys [name-token initializer]}]
  (let [env (if initializer
              (evaluate env initializer)
              env)
        res (when initializer (get-result env))]
    (define-env env name-token res)))

(defn interpret [stmts envirement]
  (reduce (fn [ctx stmt]
            (execute ctx stmt))
          envirement
          stmts))



(comment
  (defn- ev [s]
    (evaluate
     (:clox.parser/ast-node (clox.parser/parse (:tokens (clox.scanner/scanner s))))))

  (ev "5 != 4")

  (ev "\"4\" + 5")

  (/ 5 0.)


  )
