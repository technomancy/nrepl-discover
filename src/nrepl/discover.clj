(ns nrepl.discover
  (:require [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.misc :as m]
            [clojure.tools.trace :as trace]
            [clojure.test :as test]
            [clojure.walk :as w]))

(declare discover)

(def ops
  (delay (into {"discover" #'discover}
               (for [n (all-ns)
                     [_ v] (ns-publics n)
                     :when (:nrepl/op (meta v))]
                 [(:name (:nrepl/op (meta v))) v]))))

(defn- ^{:nrepl/op {:name "discover"
                    :doc "Discover known operations."}}
  discover [{:keys [transport] :as msg}]
  (t/send transport (m/response-for msg :status :done :value
                                    (for [[_ op-var] @ops]
                                      (-> op-var meta :nrepl/op
                                          w/stringify-keys)))))

(defn wrap-discover [handler]
  (fn [msg] ((@ops (:op msg) handler) msg)))

;; example nrepl op for tools.trace
(defn ^{:nrepl/op {:name "toggle-trace" :args [["var" "var" "Trace: "]]
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
                                            :message (str var " untraced."))))
          (do (trace/trace-var* v)
              (send transport (response-for msg :status :done
                                            :message (str var " traced.")))))
        (send transport (response-for msg :status #{:error :done}
                                      :message "no such var")))
      (catch Exception e
        (#'trace/tracer "Error" (.getMessage e))
        (send transport (response-for msg :status #{:error :done}
                                      :message (.getMessage e)))))))

;; example nrepl op for clojure.test
(defn report [old-report event]
  (when-let [current-test (last test/*testing-vars*)]
    (alter-meta! current-test
                 assoc :status (conj (:status (meta current-test))
                                     [(:type event) (:message event)
                                      (str (:expected event))
                                      (str (:actual event))
                                      ((test/file-position 2) 1)])))
  (old-report event))

(defn test-results [namespace]
  (filter :status (map meta (vals (ns-interns (symbol namespace))))))

(defn overlay-for [{:keys [status line]}]
  (cond (some (comp #{:fail} first) status) ["red" line ""]
        (some (comp #{:error} first) status) ["orange" line ""]
        :else ["green" line ""]))

(defn ^{:nrepl/op {:name "run-tests"
                   :doc "Run tests for a namespace"}}
  run-tests
  [{:keys [transport ns] :as msg}]
  (let [send (resolve 'clojure.tools.nrepl.transport/send)
        response-for (resolve 'clojure.tools.nrepl.misc/response-for)]
    (try
      (send transport (response-for msg :clear-overlay "true"))
      (let [summary (with-redefs [test/report (partial report test/report)]
                      (test/run-tests (symbol ns)))
            message (if (pos? (+ (:error summary) (:fail summary)))
                      "Failed" "Passed")]
        (doseq [r (test-results ns)]
          (send transport (response-for msg :overlay (overlay-for r))))
        (send transport (response-for msg :status :done
                                      :message message)))
      (catch Exception e
        (#'trace/tracer "Error" (.getMessage e))
        (send transport (response-for msg :status #{:error :done}
                                      :message (.getMessage e)))))))

;; TODO: run a single test with fixtures
;; TODO: next/prev failure? (might need hard-coding)
