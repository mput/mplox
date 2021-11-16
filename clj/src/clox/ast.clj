(ns clox.ast)

;; TODO: create macros to generate new node.

(def ast
  {:expr/assign [:name-token :value]
   :expr/logical [:left :operator :right]
   :expr/call [:calle-expr :paren-token :arguments-exprs]
   :expr/binary [:left :operator :right]
   :expr/unary [:operator :right]
   :expr/grouping [:expression]
   :expr/literal [:value]
   :expr/variable [:name-token]

   :stmt/expression [:expression]
   :stmt/block [:statements]
   :stmt/if [:condition-expr :then-stmt :else-stmt]
   :stmt/while [:condition-expr :body]
   :stmt/print [:expression]
   :stmt/var [:name-token :initializer]
   :stmt/fun [:name-token :params :body]
   :stmt/return [:keyword-token :value-expr]})

(defn new
  [type & operands]
  (assoc (->> (map vector (get ast type) operands)
              (into {}))
         :type type))
