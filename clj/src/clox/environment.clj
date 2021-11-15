(ns clox.environment
  (:require [clox.errors :as errors]))


(defn create-env
  ([] (create-env nil))
  ([parent]
   (atom (cond-> {}
           parent (assoc ::parent parent)))))

(defn define-env! [env ename val]
  (swap! env assoc-in [::values ename] val))


(defn get-env [env name-token]
  (let [v (get-in @env [::values (:lexeme name-token)] ::not-found)]
    (if (= v ::not-found)
      (if-let [parent (::parent @env)]
        (get-env parent name-token)
        (throw (errors/runtime-error name-token
                                     (str "Undefined variable '"
                                          (:lexeme name-token)
                                          "'."))))
      v)))

(defn set-env! [env name-token val]
  (if (nil? env)
    (throw (errors/runtime-error name-token
                                 (str "Undefined variable '"
                                      (:lexeme name-token)
                                      "'.")))
    (let [v (get-in @env [::values (:lexeme name-token)] ::not-found)]
      (if (= v ::not-found)
        (set-env! (::parent @env) name-token val)
        (swap! env assoc-in [::values (:lexeme name-token)] val)))))


(defn enclose-env [{env ::local :as ctx}]
  (assoc ctx ::local (create-env env)))

(defn denclose-env [{env ::local :as ctx}]
  (assoc ctx ::local (::parent @env)))
