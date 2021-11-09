(ns clox.main
  (:require
   [clox.scanner :as scanner]
   [clox.parser :as parser]
   [clox.interpreter :as interpreter]
   [clojure.pprint]))

(defn run
  ([source] (run source {}))
  ([source environment]
   (let [{:keys [tokens errors]} (scanner/scanner source)]
     (if (seq errors)
       (do
         (println "Errors in scanner!")
         (doseq [err errors]
           (println err))
         ::scanner-error)
       (let [{::parser/keys [statements errors]} (parser/parse tokens)]
         (if (seq errors)
           (do
             (println "Parsed expression:")
             (doseq [err errors]
               (println err))
             ::parser-error)
           (try (interpreter/interpret statements environment)
                (catch Throwable e
                  (println (ex-message e))
                  (clojure.pprint/pprint e)
                  ::runtime-error))))))))

(comment
  (run "print 4.1 * (2 + 3);")
  (run "print 4.1;")

  )

(defn run-prompt []
  (loop [environment {}]
    (print "> ") (flush)
    (let [new-environment
          (loop [source ""]
            (let  [code-line (read-line)]
              (case code-line
                nil (do
                      (println "Good Bay!")
                      (System/exit 0))
                "" (run source environment)
                (recur (str source code-line "\n")))))]
      (recur (if (keyword? new-environment)
               environment
               new-environment)))))

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
