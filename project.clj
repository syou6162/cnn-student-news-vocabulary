(defproject cnn-student-news-vocabulary "0.0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-http "1.0.1"]
                 [edu.stanford.nlp/stanford-parser "2.0.5"]]
  :jvm-opts ["-Xmx3g" "-server" "-Dfile.encoding=UTF-8"])
