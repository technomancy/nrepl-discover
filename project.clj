(defproject nrepl-discover "0.1.0"
  :description "Auto discovery and description of in-editor nREPL commands."
  :dependencies [[org.clojure/tools.nrepl "0.2.3"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/data.codec "0.1.0"]]
  :repl-options {:nrepl-middleware [nrepl.discover/wrap-discover]})
