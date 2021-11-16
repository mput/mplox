(ns clox.errors)




(defn runtime-error [token msg]
  (ex-info "Runtime Error" {:type :runtime-error
                            :token token
                            :message msg}))


(defmulti report (fn [{t :type}] t))

(defn report-1 [line where message]
  (println (str "[line "  line  "] Error "  where  " : "  message)))

(defmethod report :runtime-error
  [{{:keys [line]} :token message :message}]
  (println (str message " [line "  line  "]")))

(defmethod report :resolver-error
  [{{:keys [line]} :token message :message}]
  (println (str message " [line "  line  "]")))

(defmethod report :scanner
  [{:keys [message lexeme line]}]
  (report-1 line lexeme message))

(defmethod report :parser
  [{{:keys [line lexeme]} :token message :message}]
  (report-1 line lexeme message))

(defmethod report :default
  [data]
  (println "Default error reporter: " data))
