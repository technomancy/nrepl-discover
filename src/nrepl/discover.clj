(ns nrepl.discover
  (:require [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.misc :as m]
            [clojure.tools.nrepl.middleware.session :as ses]
            [clojure.tools.trace :as trace]
            [clojure.test :as test]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.data.codec.base64 :as b64]
            [clojure.walk :as w]))

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
           (finally (pop-thread-bindings)))
      (handler msg))))

;; example nrepl op for tools.trace
(defn ^{:nrepl/op {:name "toggle-trace" :args [["var" "var" "Trace: "]]
                   :doc "Toggle tracing of a given var."}}
  nrepl-op [{:keys [transport var] :as msg}]
  (try
    (if-let [v (resolve (symbol var))]
      (if (-> v meta :clojure.tools.trace/traced)
        (do (trace/untrace-var* v)
            (t/send transport (m/response-for msg :status :done
                                              :message (str var " untraced."))))
        (do (trace/trace-var* v)
            (t/send transport (m/response-for msg :status :done
                                              :message (str var " traced.")))))
      (t/send transport (m/response-for msg :status #{:error :done}
                                        :message "no such var")))
    (catch Exception e
      (#'trace/tracer "Error" (.getMessage e))
      (t/send transport (m/response-for msg :status #{:error :done}
                                        :message (.getMessage e))))))

;; example nrepl op for clojure.test
(defn- report [old-report event]
  (when-let [current-test (last test/*testing-vars*)]
    (alter-meta! current-test
                 assoc :status (conj (:status (meta current-test))
                                     [(:type event) (:message event)
                                      (str (:expected event))
                                      (str (:actual event))
                                      ((test/file-position 2) 1)])))
  (old-report event))

(defn- test-results [namespace]
  (filter :status (map meta (vals (ns-interns (symbol namespace))))))

(defn- overlay-for [{:keys [status line]}]
  (cond (some (comp #{:fail} first) status) ["red" line]
        (some (comp #{:error} first) status) ["orange" line]
        :else ["green" line]))

(defn summary-response [msg ns summary results]
  (let [message (if (pos? (+ (:error summary) (:fail summary)))
                  "Failed" "Passed")]
    (doseq [r results]
      (t/send (:transport msg) (m/response-for msg :overlay (overlay-for r))))
    (t/send (:transport msg) (m/response-for msg :status :done
                                             :message message))))

;; TODO: stdout here is still System/out, not repl-specific *out*

(defn ^{:nrepl/op {:name "run-tests"
                   :doc "Run tests for a namespace"}}
  run-tests
  [{:keys [transport ns] :as msg}]
  (try
    (t/send transport (m/response-for msg :clear-overlays "true"))
    (let [summary (with-redefs [test/report (partial report test/report)]
                    (test/run-tests (symbol ns)))]
      (summary-response msg ns summary (test-results ns)))
    (catch Exception e
      (t/send transport (m/response-for msg :status #{:error :done}
                                        :message (.getMessage e))))))

(defn test-one-var [test-ns test-name]
  (let [v (ns-resolve (symbol test-ns) (symbol test-name))
        m (meta (:ns (meta v)))
        once-fixture-fn (test/join-fixtures (:clojure.test/once-fixtures m))
        each-fixture-fn (test/join-fixtures (:clojure.test/each-fixtures m))]
    (once-fixture-fn
     (fn []
       (when (:test (meta v))
         (each-fixture-fn (fn [] (test/test-var v))))))))

(defn test-one [ns test-name]
  (binding [test/*report-counters* (ref test/*initial-report-counters*)]
    (let [ns-obj (the-ns (symbol ns))]
      (test/do-report {:type :begin-test-ns, :ns ns-obj})
      ;; If the namespace has a test-ns-hook function, call that:
      (if-let [v (find-var (symbol (str (ns-name ns-obj)) "test-ns-hook"))]
        ((var-get v))
        ;; Otherwise, just test every var in the namespace.
        (test-one-var ns test-name))
      (test/do-report {:type :end-test-ns, :ns ns-obj}))
    (test/do-report (assoc @test/*report-counters* :type :summary))
    @test/*report-counters*))

(defn ^{:nrepl/op {:name "run-test"
                   ;; TODO: need to get the list of deftests at runtime; eep
                   :args [["test" "var" "Test: "]]
                   :doc "Run a single test."}}
  run-test
  [{:keys [transport test ns] :as msg}]
  (try
    (t/send transport (m/response-for msg :clear-overlays "true"))
    (let [summary (with-redefs [test/report (partial report test/report)]
                    (test-one ns test))]
      (summary-response msg ns summary [(meta (resolve (symbol test)))]))
    (catch Exception e
      (t/send transport (m/response-for msg :status #{:error :done}
                                        :message (.getMessage e))))))

;; TODO: next/prev failure? (might need hard-coding)

(defonce times (atom 0))

(defn ^{:nrepl/op {:name "html"
                   :args [["x" "string" "String: "]]
                   :doc "Test HTML thingy"}}
  html-op [{:keys [transport x] :as msg}]
  (swap! times inc)
  (t/send transport (m/response-for msg :content-type "text/html" :value
                                    (str "<html><body><h1>thingy</h1><p>"
                                         "<a href='/html/?x=NEW"
                                         @times "'>" x
                                         "</a></p></body></html>"))))

(defn ^{:nrepl/op {:name "jpeg"
                   :args [["file" "file" "File: "]]
                   :doc "show a jay peg"}}
  jpeg-op [{:keys [transport file] :as msg}]
  (let [baos (java.io.ByteArrayOutputStream.)
        f (io/resource "leiningen.jpg")
        _ (io/copy f baos)]
    (t/send transport (m/response-for msg :content-type "image/jpeg"
                                      :value (b64/encode (.toByteArray baos))))))


(defn ^{:nrepl/op {:name "text"
                   :doc "just some text"}}
  text-op [{:keys [transport] :as msg}]
  (t/send transport (m/response-for msg :content-type "text/plain"
                                    :value "It is clear now, as it was not at first, why Illich reacted with such horror to my saying that we should push the walls of the school building out further and further. That seemed at the time a good enough way to say that we should abolish the distinction between learning and the rest of life. Only later did I see the danger that he saw right away. Think again about the global schoolhouse, madhouse, prison. What are madhouses and prisons? They are institutions of compulsory treatment...

A global schoolhouse would be a world, which we seem to be moving toward, in which one group of people would have the right through our entire lives to subject the rest of us to various sorts of tests, and if we did not measure up, to require us to submit to various kinds of treatment, i.e. education, therapy, etc., until we did. A worse nightmare is hard to imagine.")))

(defn ^{:nrepl/op {:name "clj"
                   :doc "just some clojure source"}}
  clj-op [{:keys [transport] :as msg}]
  (t/send transport (m/response-for msg :content-type "application/clojure"
                                    :value (prn-str '(defn ohai [x] (* x x))))))
