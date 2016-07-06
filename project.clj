;
; Copyright (c) Justus Adam, Sebastian Ertel, Andres Goens 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE file.
;

(defproject yauhau "0.1.0-SNAPSHOT"
  :description "Yauhau - A plugin for the ohua compiler to enable efficient IO through automatic batching and concurrency."
  :url "https://github.com/JustusAdam/yauhau"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :deploy-repositories [["releases" {:url "https://clojars.org/repo" :creds :gpg}]]

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [bwo/monads "0.2.2"]]

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :dev      {:plugins      [[lein-junit "1.1.8"]]
                        :dependencies [[rhizome "0.2.5"]
                                       [cheshire "5.5.0"]   ; json lib
                                       [funcool/cats "1.2.0"] ; muse dep
                                       [funcool/promesa "0.6.0"]] ; muse dep


                        ; paths for Clojure test cases
                        :test-paths ["yauhau/test/clojure" "muse/test/clojure" "yauhau/experiment/clojure"]
                        :java-source-paths ["yauhau/test/java" "yauhau/experiment/java"]
                        ; we use funcool's release of muse because it has functioning support for the monad library of cats.
                        ; they did not make a release to clojars yet so we include the jar here.
                        :resource-paths ["resources/muse-0.4.0.jar"]}}

  :resource-paths ["resources/ohua-0.6.2.jar"]

  :source-paths ["yauhau/src/clojure"]
  :java-source-paths ["yauhau/src/java"]

  :jvm-opts ["-Xmx2g"]                                      ; there is no need for more than that during development (some tests run on quite a lot of data)

  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options" "-g"]

  ;:omit-source true  ; avoids .java files ending up in the generated JAR file
  ; with the extension above we must explicitly exclude java source files!
  ; we can't use the :omit-source option because it also looses the Clojure sources.
  ; I filed a bug on this: https://github.com/kumarshantanu/lein-javac-resources/issues/1
  :jar-exclusions [#"\.java$"])
