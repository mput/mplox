(ns clox.ast-printer
  (:require [clox.expression :as expr]))

(defmulti ast->lisp (fn [{t :type :as h}] t))

(defmethod ast->lisp ::expr/binary
  [{:keys [left operator right]}]
  (str "(" (:lexeme operator) " "
       (ast->lisp left) " "
       (ast->lisp right)
       ")"))

(defmethod ast->lisp ::expr/unary
  [{:keys [operator right]}]
  (str "(" (:lexeme operator) " " (ast->lisp right) ")"))

(defmethod ast->lisp ::expr/grouping
  [{:keys [expression]}]
  (ast->lisp expression))

(defmethod ast->lisp ::expr/literal
  [{:keys [value]}]
  (str value))
