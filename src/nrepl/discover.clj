(ns nrepl.discover
  (:require [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.misc :as m]
            [clojure.tools.trace :as trace]
            [clojure.walk :as w]))

(declare discover)

(def ops
  (delay (into {"discover" #'discover}
               (for [n (all-ns)
                     [_ v] (ns-publics n)
                     :when (:nrepl/op (meta v))]
                 [(:name (:nrepl/op (meta v))) v]))))

(defn- ^{:nrepl/op {:name "discover"
                    ;; TODO: uncommenting this breaks nrepl.el; augh
                    ;; :arglist []
                    :doc "Discover known operations."}}
  discover [{:keys [transport] :as msg}]
  (t/send transport (m/response-for msg :status :done :value
                                    (for [[_ op-var] @ops]
                                      (-> op-var meta :nrepl/op
                                          w/stringify-keys)))))

(defn wrap-discover [handler]
  (fn [msg] ((@ops (:op msg) handler) msg)))

;; example nrepl op for tools.trace
(defn ^{:nrepl/op {:name "toggle-trace" :arglist [["var" "var" "Trace: "]]
                   :doc "Toggle tracing of a given var."}}
  nrepl-op
  [{:keys [transport var] :as msg}]
  (require 'clojure.tools.nrepl.transport 'clojure.tools.nrepl.misc)
  (let [send (resolve 'clojure.tools.nrepl.transport/send)
        response-for (resolve 'clojure.tools.nrepl.misc/response-for)]
    (try
      (if-let [v (resolve (symbol var))]
        (if (-> v meta :clojure.tools.trace/traced)
          (do (trace/untrace-var* v)
              (send transport (response-for msg :status :done
                                            :value (str var " untraced."))))
          (do (trace/trace-var* v)
              (send transport (response-for msg :status :done
                                            :value (str var " traced.")))))
        (send transport (response-for msg :status #{:error :done}
                                      :value "no such var")))
      (catch Exception e
        (#'trace/tracer "Error" (.getMessage e))
        (send transport (response-for msg :status #{:error :done}
                                      :value (.getMessage e)))))))
