(defproject meta-csv "0.1.0-SNAPSHOT"
  :description "A smart reader for CSV files"
  :url "https://github.com/ngrunwald/meta-csv"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [de.siegmar/fastcsv "1.0.3"]
                 [com.ibm.icu/icu4j "65.1"]]
  :repl-options {:init-ns meta-csv.core}
  :profiles {:dev {:dependencies [[metosin/testit "0.4.0"]]}})
