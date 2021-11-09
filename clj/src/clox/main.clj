(ns clox.main
  (:require
   [clox.scanner :as scanner]
   [clox.parser :as parser]
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
      (let [{::parser/keys [statements errors]} (parser/parse tokens)]
        (if (seq errors)
          (do
            (println "Parsed expression:")
            (doseq [err errors]
              (println err))
            (or dont-exit-on-error? (System/exit 0)))
          (try (interpreter/interpret statements {})
               (catch Throwable e
                 (println (ex-message e))
                 (clojure.pprint/pprint e))))))))

(comment
  (run "print 4.1 * (2 + 3);" :dont-exit)


  (run "print 4.1;" :dont-exit)

  )

(defn run-prompt []
  (loop []
    (print "> ")
    (flush)
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
