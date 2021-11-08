(ns clox.ast)

(def ast
  {:expr/binary [:left :operator :right]
   :expr/unary [:operator :right]
   :expr/grouping [:expression]
   :expr/literal [:value]})

(defn new
  [type & operands]
  (assoc (->> (map vector (get ast type) operands)
              (into {}))
         :type type))
