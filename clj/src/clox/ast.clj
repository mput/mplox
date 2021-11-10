(ns clox.ast)

(def ast
  {:expr/assign [:name-token :value]
   :expr/binary [:left :operator :right]
   :expr/unary [:operator :right]
   :expr/grouping [:expression]
   :expr/literal [:value]
   :expr/variable [:name-token]

   :stmt/expression [:expression]
   :stmt/print [:expression]
   :stmt/var [:name-token :initializer]})

(defn new
  [type & operands]
  (assoc (->> (map vector (get ast type) operands)
              (into {}))
         :type type))
