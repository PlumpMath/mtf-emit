(ns mtf-emit.core
  (:gen-class))

(def sentence-regex #"\s\"(.+)\"")
(def mtf-line-regex #"\"(.+)\" =>.+")

(defn not-a-real-sentence [s]
  (= \[ (first s)))

(defn to-sentences [text]
  (remove not-a-real-sentence (map second (re-seq sentence-regex text))))

(defn get-already-translated [mtf-doc]
  (let [existing (re-seq mtf-line-regex mtf-doc)]
    (zipmap (map second existing) (map first existing))))

(def sentence-count (atom 0))
(def already-translated-count (atom 0))

(defn sentence->stub [already-translated sentence]
  (swap! sentence-count inc)
  (if (contains? already-translated sentence)
    (do
      (swap! already-translated-count inc)
      (get already-translated sentence))
    (str "\"" sentence "\" =>")))

(defn to-doc [already-translated sentences]
  (clojure.string/join "\n" (map #(sentence->stub already-translated %) sentences)))

(def langs #{"eng"})

(defn spit-if-nonempty [path content]
  (when (not= content "")
    (spit path content)
    (println (str "Emitted " path
                  "\t Sentences translated: " @already-translated-count
                  "/" @sentence-count))))

(defn convert [input-path output-dir lang]
  (assert (string? input-path))
  (assert (contains? langs lang))
  (reset! sentence-count 0)
  (reset! already-translated-count 0)
  (let [dia-name (second (re-find #"/([A-z0-9_]+).dia" input-path))
        output-path (str output-dir "/" dia-name "." lang ".mtf")
        dia-contents (slurp input-path)
        existing-mtf-doc (try (slurp output-path) (catch Exception e nil))
        already-translated (if existing-mtf-doc (get-already-translated existing-mtf-doc) [])]
    (->> dia-contents
         to-sentences
         (to-doc already-translated)
         (spit-if-nonempty output-path))))

(defn is-dia-file? [filename]
  (and (re-find #"([A-z0-9_]+)\.dia" filename)
       (not (re-find #"\.svn" filename))))

(defn get-dia-files [in-dir]
  (let [fs (file-seq (clojure.java.io/file in-dir))]
    (filter is-dia-file? (map str fs))))

(defn -main
  [& args]
  (if (not= (count args) 3)
    (throw (Exception. "Must give three command line args: <in-dir> <out-dir> <lang>"))
    (let [[in-dir out-dir lang] args
          files (get-dia-files in-dir)]
      (doseq [f files]
        (convert f out-dir lang)))))

(comment
  (-main "./dia" "./mtf" "eng"))
