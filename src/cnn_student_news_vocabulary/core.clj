(ns cnn-student-news-vocabulary.core
  (:require [clj-http.client :as client])
  (:import
   (java.io StringReader)
   (edu.stanford.nlp.ling Word)
   (edu.stanford.nlp.process
    CoreLabelTokenFactory WordTokenFactory PTBTokenizer WordToSentenceProcessor)))

(def cnn-student-news-base-url
  "http://transcripts.cnn.com/TRANSCRIPTS/sn.html")

(defn get-news-urls []
  (->> (get (client/get cnn-student-news-base-url) :body)
       (re-seq #"(?s)<div class=\"cnnSectBulletItems\">(.*?)</div>")
       (map
        (fn [s]
          (re-find #"(?s)<a href=\"(.*?)\">" (second s))))
       (map
        (fn [s]
          (str "http://transcripts.cnn.com" (second s))))))

(defn tokenize [^String text]
  (-> (PTBTokenizer.
       (StringReader. text)
       (WordTokenFactory.)
       "")
      (.tokenize)))

(defn get-content-from-html [html]
  (let [regex #"(?s)<[pP] class=\"cnnBodyText\">(.*?)</[pP]>"
        content (->> html
                     (re-seq regex)
                     (map
                      (fn [s]
                        (-> (second s)
                            (clojure.string/replace #"<br>" " ")
                            (clojure.string/replace #"\t" "")
                            (clojure.string/replace #"\n" "")
                            (clojure.string/replace #"\s+" " "))))
                     (last))]
    (->> (tokenize content)
         (map #(.word %))
         (clojure.string/join " "))))

(def stop-words
  (-> (slurp "stop_words")
      (clojure.string/split #"\r\n")
      (set)))

(defn stop-word? [w]
  (contains? stop-words w))

(defn contains-symbol? [w]
  (not (nil? (re-find #"[\.,`-]" w))))

(defn contains-number? [w]
  (not (nil? (re-find #"\d" w))))

(defn less-than-two-chars? [w]
  (> 3 (count w)))

(defn begin-with-capital-letter? [w]
  (Character/isUpperCase (nth w 0)))

(defn unnecessary-word? [w]
  (or (stop-word? w)
      (contains-symbol? w)
      (contains-number? w)
      (less-than-two-chars? w)
      (begin-with-capital-letter? w)))

(defn remove-unnecessary-words [coll]
  (->> coll
       (remove stop-word?)
       (remove contains-symbol?)
       (remove contains-number?)
       (remove less-than-two-chars?)
       (remove begin-with-capital-letter?)))

(let [cache-dir (java.io.File. "cache")]
  (when-not (.exists cache-dir)
    (.mkdir cache-dir))
 (defn get-cached-content [orig-url]
   (let [url (-> orig-url
                 (clojure.string/replace #":" "_")
                 (clojure.string/replace #"/" "_"))
         cache-file (str cache-dir "/" url)
         file (java.io.File. cache-file)]
     (if (.exists file)
       (slurp file)
       (let [content (-> (client/get orig-url)
                         (get :body))]
         (spit file content)
         content)))))

(defn extract-word-entries [url]
  (let [body (get-cached-content url)
        title (second (re-find #"<title>(.*?)</title>" body))]
    (binding [*out* *err*]
      (println (str "Fetching " url "...")))
    (let [content (get-content-from-html body)]
      (->> (clojure.string/split content #" ")
           ; (map clojure.string/lower-case)
           (map
            (fn [w]
              {:title title
               :url url
               :word w
               :content content}))))))

(def cnn-words
  (let [urls (get-news-urls)]
    (->> urls
         ; (take 3)
         (map extract-word-entries)
         (reduce into [])
         (remove (fn [w] (unnecessary-word? (:word w)))))))

(def word2usage
  (->> cnn-words
       (group-by :word)
       (into {})))

(def word2freq
  (->> cnn-words
       (group-by :word)
       (sort-by
        (fn [[w coll]]
          (count coll))
        >)
       (map
        (fn [[w coll]]
          [w (count coll)]))
       (into {})))

(defn slurp-word2freq [filename]
  (binding [*out* (java.io.FileWriter. filename)]
    (doseq [[k v] (->> word2freq
                       (sort-by second >))]
      (println (str k "\t" v)))))

(slurp-word2freq "word2freq.txt")

(def my-unknown-words
  #{"craters"
    "wreckage"
    "measles"
    "droughts"
    "vaccinated"
    "radar"
    "separatists"
    "sanctions"
    "insanity"
    "defendant"
    "urchins"
    "soaring"
    "dwarf"
    "aircraft"
    "outbreak"
    "symptoms"
    "haven"
    "combat"
    "terror"
    "spacecraft"
    "lawmakers"
    "presidential"
    "orbit"
    "diagnosed"
    "aviation"
    "communist"
    "sweeteners"
    "dash"
    "coalition"
    "cliff"
    "surveillance"
    "fiscal"
    "parliament"
    "presidency"
    "surge"
    "ballot"
    "devastating"
    "inauguration"
    "faith"
    "ballots"
    "ambassador"
    "terrain"
    "lawsuit"
    "diplomatic"
    "rally"
    "extraordinary"
    "postal"
    "desperate"
    "dedication"
    "armistice"
    "explosives"
    "austerity"
    "pledge"})

(defn post-word [word]
  (doseq [w (->> (get word2usage word)
                 (group-by :url)
                 (map
                  (fn [[url coll]]
                    (first coll))))]
    (let [m {:title (-> w :title)
             :url (-> w :url)
             :word (-> w :word)
             :content (-> w :content)}
          result (client/post "http://localhost:8080/" {:form-params m})]
      (if (= 200 (:status result))
        (println (str "Posted " (:word w) " (" (:url w) ")..."))
        (println (str "Failed posting " w " (" (:url w) ")..."))))))

(doseq [w my-unknown-words]
  (post-word w))
