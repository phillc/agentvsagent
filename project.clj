(defproject agent-vs-agent "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2760"]
                 [org.omcljs/om "0.8.8"]]
  :plugins [[lein-cljsbuild "1.0.6"]]
  :hooks [leiningen.cljsbuild]
  :source-paths ["src/clj"]
  :cljsbuild {
              :builds {
                       :main {
                              :source-paths ["web/app"]
                              :compiler {:output-to "web/public/javascripts/app.js"
                                         :optimizations :simple
                                         :pretty-print true}
                              :jar true}}}
  )
