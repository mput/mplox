(ns clox.callable)

(defprotocol LoxCallable
  "Anything that could be called in lox language."
  (call [callee env arguments])
  (arity [callee])
  (toString [callee]))
