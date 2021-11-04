(ns clox.expression)

(def expression
  {::binary [:left :operator :right]
   ::grouping [:expression]
   ::literal [:value]
   ::unary [:operator :right]})

(defn new
  [type & operands]
  (assoc (->> (map vector (get expression type) operands)
              (into {}))
         :type type))
