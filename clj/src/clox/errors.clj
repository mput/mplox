(ns clox.errors)

(defn runtime-error [token msg]
  (ex-info "Runtime Error" {:type :runtime-error
                            :token token
                            :message msg}))
