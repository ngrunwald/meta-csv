(defproject meta-csv "0.1.0"
  :description "A smart reader for CSV files"
  :url "https://github.com/ngrunwald/meta-csv"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}
  :repl-options {:init-ns meta-csv.core}
  :profiles {:dev {:dependencies [[metosin/testit "0.4.0"]]}})
