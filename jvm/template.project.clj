;;;
;;; Copyright (c) Sebastian Ertel 2015. All Rights Reserved.
;;;
;;;This source code is licensed under the terms described in the associated LICENSE.TXT file.
;;;
(defproject yauhau "0.7.2"
  :description "Ohua - An EDSL for implicit parallel programming of stateful computations that execute on a dataflow runtime system."
  ;:url "https://github.com/ohua-dev/ohua-jvm-integration"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [ohua-jvm-runtime "0.1-SNAPSHOT"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :resource-paths [
    "resources/ohua-jvm-runtime-0.1-SNAPSHOT.jar"
    (- insert-jar-deps -)
                   ]
  :test-paths ["test/clojure"]
  )
