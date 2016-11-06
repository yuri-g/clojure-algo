(defproject algo "0.1.0"
  :description "Various algorithms implemented in Clojure."
  :url "https://github.com/yuri-g/clojure-algo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :plugins [[lein-kibit "0.1.2"]]
  :main algo.core
  :aot [algo.core])
