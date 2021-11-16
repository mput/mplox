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

(defn get-scope-at [env depth]
  (if (zero? depth)
    env
    (recur (::parent @env) (dec depth))))

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

(defn set-env-at! [env* depth name-token val]
  (let [env (get-scope-at env* depth)
        v (get-in @env [::values (:lexeme name-token)] ::not-found)]
    (if (= v ::not-found)
      (throw (ex-info "Impossible missing var." {}))
      (swap! env assoc-in [::values (:lexeme name-token)] val))))


(defn get-env-at [env name-token depth]
  (let [v (get-in @(get-scope-at env depth)
                  [::values (:lexeme name-token)]
                  ::not-found)]
    (if (= v ::not-found)
      (throw (ex-info "Impossible missing var." {}))
      v)))


(defn enclose-env [{env ::local :as ctx}]
  (assoc ctx ::local (create-env env)))

(defn denclose-env [{env ::local :as ctx}]
  (assoc ctx ::local (::parent @env)))
