(ns clox.expression)

(def expression
  {::binary [:left :operator :right]
   ::grouping [:expression]
   ::literal [:value]
   ::unare [:operator :right]})

(defn new-expr
  [type & operands]
  (assoc (->> (map vector (get expression type) operands)
              (into {}))
         :type type))


;; [Trace - 11:02:05 PM] Sending request 'textDocument/rename - (403)'.
;; Params: {
;;   "textDocument": {
;;     "uri": "file:///Users/mput/projects/clox/clj/src/clox/expression.clj"
;;   },
;;   "position": {
;;     "line": 10,
;;     "character": 31
;;   },
;;   "newName": "expressiond"
;; }


;; [Trace - 11:02:05 PM] Received response 'textDocument/rename - (403)' in 3ms.
;; Result: null


;; [Trace - 11:02:06 PM] Sending request 'textDocument/codeAction - (404)'.
;; Params: {
;;   "textDocument": {
;;     "uri": "file:///Users/mput/projects/clox/clj/src/clox/expression.clj"
;;   },
;;   "range": {
;;     "start": {
;;       "line": 10,
;;       "character": 31
;;     },
;;     "end": {
;;       "line": 10,
;;       "character": 31
;;     }
;;   },
;;   "context": {
;;     "diagnostics": []
;;   }
;; }
