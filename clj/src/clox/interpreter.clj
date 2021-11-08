(ns clox.interpreter
  (:require [clox.scanner :as scanner]
            [clojure.string :as str]))

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

(defn- runtime-error [token msg]
  (ex-info msg {:type :runtime-error
                :token token}))

(defn- check-number-operand [operator operand]
  (when-not (double? operand)
    (throw (runtime-error operator "Operand must be a number."))))

(defn- check-number-operands [operator left right]
  (when-not (and (double? left) (double? right))
    (throw (runtime-error operator "Operand must be a number."))))

(defmulti evaluate (fn [{t :type}] t))

(defmethod evaluate :expr/binary
  [{:keys [left operator right]}]
  (let [l-res (evaluate left)
        r-res (evaluate right)]
    (case (:type operator)
      ::scanner/plus
      (cond
        (and (number? l-res) (number? r-res))
        (+ (double l-res) (double r-res))

        (and (string? l-res) (string? r-res))
        (str l-res r-res)
        :else (throw
               (runtime-error operator "Operands must be two numbers or two strings.")))

      ::scanner/minus
      (do
        (check-number-operands operator l-res r-res)
        (- (double l-res) (double r-res)))

      ::scanner/star
      (do
        (check-number-operands operator l-res r-res)
        (* (double l-res) (double r-res)))

      ::scanner/slash
      (do
        (check-number-operands operator l-res r-res)
        (/ (double l-res) (double r-res)))

      ::scanner/greater
      (do
        (check-number-operands operator l-res r-res)
        (> (double l-res) (double r-res)))

      ::scanner/greater-equal
      (do
        (check-number-operands operator l-res r-res)
        (>= (double l-res) (double r-res)))

      ::scanner/less
      (do
        (check-number-operands operator l-res r-res)
        (< (double l-res) (double r-res)))

      ::scanner/less-equal
      (do
        (check-number-operands operator l-res r-res)
        (<= (double l-res) (double r-res)))

      ::scanner/equal-equal
      (do
        (check-number-operands operator l-res r-res)
        (equal?' (double l-res) (double r-res)))

      ::scanner/bang-equal
      (do
        (check-number-operands operator l-res r-res)
        (not (equal?' (double l-res) (double r-res))))

      )))

(defmethod evaluate :expr/unary
  [{:keys [operator right]}]
  (let [res (evaluate right)]
    (case (:type operator)
      ::scanner/minus
      (do
        (check-number-operand operator res)
        (- (double res)))

      ::scanner/bang
      (not (trusy? res)))))

(defmethod evaluate :expr/grouping
  [{:keys [expression]}]
  (evaluate expression))

(defmethod evaluate :expr/literal
  [{:keys [value]}]
  value)

(defn strinfigy [val]
  (cond
    (nil? val) "null"

    (double? val)
    (if (str/ends-with? (str val) ".0")
      (subs (str val) 0 (- (count (str val)) 2))
      (str val))

    :else (str val)))


;; (defn interpret [expr]
;;   (let [res (evaluate expr)]
;;       (println (strinfigy res)))
;;   )



(comment
  (defn- ev [s]
    (evaluate
     (:clox.parser/ast-node (clox.parser/parse (:tokens (clox.scanner/scanner s))))))

  (ev "5 != 4")

  (ev "\"4\" + 5")

  (/ 5 0.)


  )
