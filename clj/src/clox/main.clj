(ns clox.main
  (:require
   [clox.scanner :as scanner]
   [clox.parser :as parser]
   [clox.ast-printer :as ast-printer]
   [clox.interpreter :as interpreter]
   [clojure.pprint]))

(defn run [source & [dont-exit-on-error?]]
  (let [{:keys [tokens errors]} (scanner/scanner source)]
    (if (seq errors)
      (do
        (println "Errors in scanner!")
        (doseq [err errors]
          (println err))
        (or dont-exit-on-error? (System/exit 0)))
      (let [{::parser/keys [ast-node errors]} (parser/parse tokens)]
        (if (seq errors)
          (do
            (println "Parsed expression:")
            (doseq [err errors]
              (println err))
            (or dont-exit-on-error? (System/exit 0)))
          (try (println (interpreter/strinfigy (interpreter/evaluate ast-node)))
               (catch Throwable e
                 (println (ex-message e))
                 (clojure.pprint/pprint (ex-data e)))))))))

(defn ev-str [s]
  (interpreter/strinfigy
   (interpreter/evaluate
     (:clox.parser/ast-node (parser/parse (:tokens (scanner/scanner s)))))))

(comment
  (ev-str "4 * (2 + 3)")

  )

(defn run-prompt []
  (loop []
    (println ">  ")
    (loop [source ""]
      (let  [code-line (read-line)]
        (case code-line
          nil (do
                (println "GoodBay!")
                (System/exit 0))
          "" (run source true)
          (recur (str source code-line "\n")))))
    (recur)))

(defn run-file [src]
  (let [source (slurp src)]
    (run source)))

(defn -main
  [& args]
  (cond
    (zero? (count args)) (run-prompt)
    (= 1 (count args)) (run-file (first args))
    :else (do
            (println "Usage: clox [src]")
            (System/exit 64))))
