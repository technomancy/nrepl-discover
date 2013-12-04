(ns nrepl.discover
  "Middleware to discover and intercept nREPL ops in loaded namespaces."
  (:require [clojure.tools.nrepl.misc :as m]
            [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.middleware.session :as ses]))

(declare discover)

(defn ops []
  (into {"discover" #'discover}
        (for [n (all-ns)
              [_ v] (ns-publics n)
              :when (:nrepl/op (meta v))]
          [(:name (:nrepl/op (meta v))) v])))

(defn- ^{:nrepl/op {:name "discover"
                    :doc "Discover known operations."}}
  discover [{:keys [transport] :as msg}]
  (t/send transport (m/response-for msg :status :done :value
                                    (for [[_ op-var] (ops)]
                                      (-> op-var meta :nrepl/op)))))

(defn ^{:clojure.tools.nrepl.middleware/descriptor {:requires #{#'ses/session}}}
  wrap-discover [handler]
  (fn [{:keys [op session] :as msg}]
    (if-let [discovered-handler ((ops) op)]
      (try (push-thread-bindings @session)
           (discovered-handler msg)
           ;; TODO: send exception back to client
           (finally (pop-thread-bindings)))
      (handler msg))))
