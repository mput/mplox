(ns clox.expression)

(def expression
  {::binary [:left :operator :right]
   ::unary [:operator :right]
   ::grouping [:expression]
   ::literal [:value]})

(defn new
  [type & operands]
  (assoc (->> (map vector (get expression type) operands)
              (into {}))
         :type type))
