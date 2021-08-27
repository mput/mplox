(ns clox.main
  (:require
   [clox.scanner :as scanner]))

(defn run [source]
  (let [{:keys [tokens errors]} (scanner/scanner source)]
    (doseq [error errors]
      (println error))
    (doseq [token tokens]
      (println token))))

(defn run-prompt []
  (loop []
    (println ">  ")
    (loop [source ""]
      (let  [code-line (read-line)]
        (case code-line
          nil (do
                (println "GoodBay!")
                (System/exit 0))
          "" (run source)
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
