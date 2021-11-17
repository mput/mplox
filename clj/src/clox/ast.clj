(ns clox.ast)

;; TODO: create macros to generate new node.

(def ast
  {:expr/assign [:name-token :value]
   :expr/logical [:left :operator :right]
   :expr/set [:object-expr :name-token :value]
   :expr/call [:calle-expr :paren-token :arguments-exprs]
   :expr/get [:object-expr :name-token]
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
   :stmt/class [:name-token :methods]
   :stmt/return [:keyword-token :value-expr]})

(defn new
  [type & operands]
  (->> (map vector (get ast type) operands)
       (into {:type type})))
