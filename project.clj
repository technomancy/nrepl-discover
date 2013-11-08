(defproject nrepl-discover "0.0.1-SNAPSHOT"
  :description "nREPL auto-discovery proof-of-concept."
  :dependencies [[org.clojure/tools.nrepl "0.2.3"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/clojure "1.5.1"]]
  :repl-options {:nrepl-middleware [nrepl.discover/wrap-discover]})
