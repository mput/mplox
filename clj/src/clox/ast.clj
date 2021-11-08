(ns clox.ast)

(def ast
  {:expr/binary [:left :operator :right]
   :expr/unary [:operator :right]
   :expr/grouping [:expression]
   :expr/literal [:value]
   :expr/variable [:name]

   :stmt/expression [:expression]
   :stmt/print [:expression]
   :stmt/var [:name :initializer]})

(defn new
  [type & operands]
  (assoc (->> (map vector (get ast type) operands)
              (into {}))
         :type type))
