(ns meta-csv.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            #?@(:bb [[clojure.data.csv :as csv]]
                :clj [[clojure.spec.alpha :as s]]))
  (:import [java.io Reader BufferedReader]
           #?@(:bb []
               :clj [[de.siegmar.fastcsv.reader CsvReader CsvRow CsvParser]
                     [de.siegmar.fastcsv.writer CsvWriter]
                     [com.ibm.icu.text CharsetDetector]
                     [java.nio.charset Charset]])))

(def ^:no-doc default-preprocess-fn str/trim)
(def ^:no-doc default-postprocess-fn identity)
(defn ^:no-doc default-write-fn
  [v]
  (cond (keyword? v) (name v)
        (symbol? v) (name v)
        (string? v) v
        :else (str v)))

(defn ^:no-doc integer-string?
  [s]
  (boolean (re-matches #"-?\d+" s)))

(defn ^:no-doc float-string?
  [s]
  (boolean (re-matches #"-?\d[\d\p{Zs}']*([\.,][\d\p{Zs}']*\d)?" s)))

(defn ^:no-doc percent-string?
  [s]
  (boolean (re-matches #"-?\d+([\.,]\d+)?\s*%\s*$" s)))

(def ^:no-doc base-types
  {:long    {:predicate integer-string?
             :coercer #(when (> (count %) 0)
                         (Long/parseLong %))
             :priority 0}
   :double  {:predicate float-string?
             :coercer #(-> %
                           (str/replace #"[\p{Sc}\s]+" "")
                           (cond-> (not (re-find #"\." %)) (str/replace #"," "."))
                           (cond-> (re-find #"\." %) (str/replace #"\,+" ""))
                           (Double/parseDouble))
             :priority 1}
   :percent {:predicate percent-string?
             :coercer (fn [s] (let [[num-str] (re-find #"-?\d[\d\p{Zs}']*([\.,][\d\p{Zs}']*\d)?" s)
                                    num (Double/parseDouble num-str)]
                                (/ num 100.0)))
             :priority 2}
   :string  {:predicate string?
             :coercer identity
             :priority 3}})

(def ^:no-doc other-types
  {:float   {:predicate float-string?
             :coercer #?(:bb #(Double/parseDouble %)
                         :clj #(Float/parseFloat %))}
   :integer {:predicate integer-string?
             :coercer #(Integer/parseInt %)}})

(def ^:no-doc synonym-types
  {:int (:integer other-types)
   :str (:string base-types)})

(def ^:no-doc guessable-csv-types base-types)
(def ^:no-doc csv-types (merge base-types other-types synonym-types))

(def ^:no-doc available-charsets #?(:bb #{"UTF-8"}
                                    :clj (into #{} (.keySet (Charset/availableCharsets)))))

(defn ^:no-doc guess-charset
  [^java.io.InputStream is]
  #?(:bb "UTF-8" ;; use file -bi later if present to try a guess
     :clj
     (try
       (let [^CharsetDetector detector (CharsetDetector.)]
         (.enableInputFilter detector true)
         (.setText detector is)
         (let [m (.detect detector)
               encoding (.getName m)]
           (if (available-charsets encoding)
             encoding
             "UTF-8")))
       (catch Exception _
         "UTF-8"))))

(def ^:no-doc boms
  {[(byte -1) (byte -2)] [:utf16-le 2]
   [(byte -2) (byte -1)] [:utf16-be 2]
   [(byte -17) (byte -69) (byte -65)] [:utf8 3]
   [(byte -1) (byte -2) (byte 0) (byte 0)] [:utf32-le 4]
   [(byte 0) (byte 0) (byte -2) (byte -1)] [:utf32-be 4]
   [] [:none 0]})

(def ^:no-doc bom-names
  (reduce (fn
            [acc [_ [bom-name _]]]
            (conj acc bom-name))
          #{} boms))

(def ^:no-doc bom-sizes
  (reduce (fn
            [acc [_ [bom-name bom-size]]]
            (assoc acc bom-name bom-size))
          {} boms))

(def ^:no-doc bom-bytes
  (reduce (fn
            [acc [bts [bom-name _]]]
            (assoc acc bom-name bts))
          {} boms))

(defn ^:no-doc skip-bom-from-stream-if-present
  [^java.io.InputStream stream]
  (let [pbis (java.io.PushbackInputStream. stream 4)
        bom (byte-array 4)]
    (.read pbis bom)
    (let [[a b c _ :as first-four] (into [] (seq bom))
          first-two [a b]
          first-three [a b c]
          [bom-name bom-size] (or (boms first-two) (boms first-three) (boms first-four))]
      (if bom-size
        (let [to-put-back (byte-array (drop bom-size bom))]
          (.unread pbis to-put-back)
          [(io/input-stream pbis) bom-name])
        (do
          (.unread pbis bom)
          [(io/input-stream pbis) :none])))))

(defn ^:no-doc get-reader
  ([src encoding bom]
     (if (instance? Reader src)
       [src (fn [& _] nil) encoding nil]
       (let [[^java.io.InputStream raw-stream close-fn] (if (instance? java.io.InputStream src)
                                                          [src (fn [& _] nil)]
                                                          (let [istream (io/input-stream src)]
                                                            [istream
                                                             #?(:bb (fn [& _] nil)
                                                                :clj (fn [& args]
                                                                       (.close istream)
                                                                       (doseq [^java.lang.AutoCloseable arg args]
                                                                         (.close arg))
                                                                       true))]))
             enc (or encoding (guess-charset raw-stream))
             [^java.io.InputStream istream bom-name] (if (nil? bom)
                                               (skip-bom-from-stream-if-present raw-stream)
                                               (do (.read raw-stream (byte-array (get bom-sizes bom 0)))
                                                   [raw-stream bom]))
             rdr (io/reader istream :encoding enc)]
         [rdr close-fn enc bom-name])))
  ([src encoding] (get-reader src encoding nil))
  ([src] (get-reader src nil nil)))

#?(:bb nil
   :clj
   (do
     (defmacro ^:private ^:no-doc sdef-enum
       [nam enum]
       `(s/def ~nam ~(eval enum)))

     (s/def :meta-csv.core.field/field (s/or :string string? :keyword keyword?))
     (sdef-enum :meta-csv.core.field/type (into #{} (keys csv-types)))
     (s/def :meta-csv.core.field/preprocess-fn fn?)
     (s/def :meta-csv.core.field/postprocess-fn fn?)
     (s/def :meta-csv.core.field/write-fn fn?)
     (s/def :meta-csv.core.field/skip? boolean?)

     (s/def ::field-definition (s/or
                                :full-spec (s/keys :opt-un [:meta-csv.core.field/field
                                                            :meta-csv.core.field/type
                                                            :meta-csv.core.field/preprocess-fn
                                                            :meta-csv.core.field/postprocess-fn
                                                            :meta-csv.core.field/write-fn
                                                            :meta-csv.core.field/skip?])
                                :field-name :meta-csv.core.field/field
                                :skip-field nil?))
     (s/def ::fields-definition-list (s/coll-of ::field-definition :min-count 1))))

(defn ^:no-doc is-header?
  [line schema]
  (if (> (count (filter nil? line)) 0)
    false
    (loop [todo schema
           idx 0]
      (if-let [{:keys [type]} (first todo)]
        (let [v (get line idx)
              {:keys [predicate]} (csv-types type)]
          (if (#{:string :str} type)
            (recur (rest todo) (inc idx))
            (if (predicate v)
              (recur (rest todo) (inc idx))
              true)))
        false))))

(defn ^:no-doc is-quoted?
  [lines delimiter]
  (let [quoted-delim (java.util.regex.Pattern/quote (str delimiter))
        regexp (re-pattern (str (format "\"%s|%s\"" quoted-delim quoted-delim)))]
    ;; skip the first line in case it's a header
    (every? #(re-find regexp %) (take 10 (rest lines)))))

(defn ^:no-doc find-char-pos
  [^String line char]
  (loop [found []
         cur 0]
    (let [pos (.indexOf line (int char) cur)]
      (if (not= pos -1)
        (recur (conj found pos) (inc pos))
        found))))

(defn ^:no-doc guess-delimiter
  [lines]
  (let [all-dels (for [line lines
                       :let [clean-line (str/replace line #"\"[^\"]*\"" "")]]
                   (into {}
                         (map
                          (fn [character]
                            [character
                             (count (find-char-pos clean-line character))])
                          [\, \; \space \tab \|])))
        freqs (first all-dels)
        report (loop [todo all-dels
                      candidates (into {}
                                       (map (fn [k] [k 0]) (keys (first all-dels))))]
                 (if-let [dels (first todo)]
                   (let [diffs (filter
                                (fn [[k v]] (or (= v 0) (not= v (freqs k))))
                                dels)]
                     (recur (rest todo) (reduce (fn [acc k]
                                                  (update-in acc [k] #(if % (inc %) 1)))
                                                candidates (map first diffs))))
                   candidates))
        [[fc fv] [_ sv] & _] (sort-by (fn [[_ v]] v) report)]
    (when (or (<= fv sv) (nil? sv))
      fc)))

#?(:bb
   nil
   :clj
   (defn ^:no-doc make-csv-reader
     ^CsvReader [{:keys [delimiter skip-empty-rows? header?
                         text-delimiter error-on-different-field-count?]}]
     (let [^CsvReader reader (CsvReader.)]
       (when delimiter (.setFieldSeparator reader delimiter))
       (when skip-empty-rows? (.setSkipEmptyRows reader true))
       (when header? (.setContainsHeader reader true))
       (when text-delimiter (.setTextDelimiter reader text-delimiter))
       (when error-on-different-field-count? (.setErrorOnDifferentFieldCount reader true))
       reader)))

(defn ^:no-doc row->clj
  [#?@(:bb  [row]
       :clj [^CsvRow row])
   {:keys [fields field-names-fn named-fields? skip null]
    :or {skip 0}}]
  (try
    (loop [todo fields
           row-idx 0
           idx 0
           acc (transient [])]
      (if (seq todo)
        (let [{:keys [field type skip? postprocess-fn preprocess-fn]
               :or {postprocess-fn default-postprocess-fn
                    preprocess-fn default-preprocess-fn} :as spec} (first todo)]
          (if (or skip? (nil? spec))
            (recur (rest todo) (inc row-idx) idx acc)
            (let [kname (if named-fields? (field-names-fn field) idx)
                  ^String v #?(:bb (row row-idx)
                               :clj (.getField row row-idx))
                  preproc-v (preprocess-fn v)
                  filter-v (if (and null preproc-v
                                    (if (set? null) (null preproc-v) (= preproc-v null)))
                             nil
                             preproc-v)
                  {:keys [coercer]} (type csv-types)
                  coerced (if (and filter-v (not (.isEmpty filter-v)))
                            (coercer preproc-v)
                            nil)
                  transformed (when coerced (postprocess-fn coerced))]
              (recur (rest todo)
                     (inc row-idx)
                     (inc idx)
                     (if named-fields?
                       (-> acc (conj! kname) (conj! transformed))
                       (assoc! acc kname transformed))))))
        (with-meta
          (if named-fields?
            (apply array-map (persistent! acc))
            (persistent! acc))
          #?(:bb {}
             :clj {::original-line-number (+ skip (.getOriginalLineNumber row))}))))
    (catch Exception e
      (let [line-number #?(:bb nil
                           :clj (+ skip (.getOriginalLineNumber row)))]
        (throw (ex-info (format "Error reading row %d" line-number)
                        {:line-number line-number}
                       e))))))

(defn ^:no-doc parse-fields
  [lines delimiter]
  (let [txt (str/join "\n" lines)]
    (with-open [rdr (java.io.StringReader. txt)]
      #?(:bb
         (doall (csv/read-csv rdr :delimiter delimiter))
         :clj
         (let [csv-rdr (make-csv-reader {:delimiter delimiter})
               seg-lines (map (fn [^CsvRow row] (into [] (.getFields row))) (.getRows (.read csv-rdr rdr)))]
           seg-lines)))))

(defn ^:no-doc take-higher-priority
  [cands]
  (if (empty? cands)
    {:type :string}
    (key (first (sort-by (comp :priority val) cands)))))

(defn ^:no-doc guess-types
  [lines]
  (for [offset (range (count (first lines)))]
    (loop [todo lines
           candidates guessable-csv-types
           not-nil 0]
      (if-let [line (first todo)]
        (let [field (nth line offset)
              valid (reduce (fn [acc [type {:keys [predicate] :as cand}]]
                              (if (or (nil? field) (predicate field))
                                (assoc acc type cand)
                                acc))
                            {} candidates)
              new-nil (if (nil? field) not-nil (inc not-nil))]
          (recur (rest todo) valid new-nil))
        (if (> not-nil (* 0.5 (count lines)))
          {:type (take-higher-priority candidates)}
          {:type :string})))))

(defn ^:no-doc analyze-csv
  [uri lookahead]
  (when (instance? Reader uri)
    (let [^Reader rdr uri]
      (if (.markSupported rdr)
        (.mark rdr 1000000)
        (throw (Exception. "Cannot analyze csv from unmarkable reader")))))
  (let [^BufferedReader rdr (io/reader uri)]
    (try
      (let [lines (loop [ls []]
                    (if-let [line (.readLine rdr)]
                      (if (< (count ls) lookahead)
                        (recur (conj ls line))
                        ls)
                      ls))
            delimiter (guess-delimiter lines)
            seg-lines (parse-fields lines delimiter)
            fields-schema (guess-types (rest seg-lines))
            has-header? (is-header? (first seg-lines) fields-schema)
            quoted? (is-quoted? lines delimiter)]
        (when (instance? java.io.Reader uri)
          (let [^Reader typed-rdr uri]
            (.reset typed-rdr)))
        {:delimiter delimiter :fields fields-schema :header? has-header? :quoted? quoted?
         :possible-header (first seg-lines)})
      (finally
        (if-not (instance? Reader uri)
          (.close rdr))))))

(defn ^:no-doc normalize-fields-schema
  [schema]
  (cond
    (sequential? schema) (mapv (fn [x] (cond
                                         (keyword? x) {:field x}
                                         (string? x) {:field x}
                                         :else x))
                               schema)
    (map? schema) schema
    :else nil))

(defn ^:no-doc normalize-csv-options
  [{:keys [field-names-fn fields] :as opts}]
  (-> opts
      (cond-> (not field-names-fn) (assoc :field-names-fn identity))
      (cond-> fields (assoc :fields (normalize-fields-schema (:fields opts))))))

(defn ^:no-doc defined-name?
  [v]
  (cond (string? v) (> (.length ^String v) 0)
        (keyword? v) true
        :else false))

#?(:bb nil
   :clj
   (do
     (s/def :meta-csv.core.reader-option/header? boolean?)
     (s/def :meta-csv.core.reader-option/sample-size integer?)
     (s/def :meta-csv.core.reader-option/guess-types? boolean?)
     (s/def :meta-csv.core.reader-option/skip integer?)
     (s/def :meta-csv.core.reader-option/null (s/or :string string?
                                                    :set set?))

     (s/def :meta-csv.core.reader-option/fields ::fields-definition-list)

     (sdef-enum :meta-csv.core.reader-option/encoding (into #{} (map str/lower-case available-charsets)))
     (sdef-enum :meta-csv.core.reader-option/bom bom-names)

     (s/def :meta-csv.core.reader-option/field-names-fn fn?)

     (s/def :meta-csv.core.reader-option/delimiter #(instance? Character %))

     (s/def ::guess-spec-args (s/keys :opt-un [:meta-csv.core.reader-option/header?
                                               :meta-csv.core.reader-option/sample-size
                                               :meta-csv.core.reader-option/guess-types?
                                               :meta-csv.core.reader-option/skip
                                               :meta-csv.core.reader-option/fields
                                               :meta-csv.core.reader-option/encoding
                                               :meta-csv.core.reader-option/bom
                                               :meta-csv.core.reader-option/field-names-fn
                                               :meta-csv.core.reader-option/delimiter
                                               :meta-csv.core.reader-option/null]))

     (s/def ::csv-source (s/or :java-reader #(instance? Reader %)
                               :java-input-stream #(instance? java.io.InputStream %)
                               :uri string?))

     (s/def :meta-csv.core.reader-option/skip-analysis? boolean?)
     (s/def ::read-csv-args (s/merge
                             ::guess-spec-args
                             (s/keys :opt-un [:meta-csv.core.reader-option/skip-analysis?])))

     (s/fdef guess-spec
       :args (s/cat :input ::csv-source
                    :options (s/? ::guess-spec-args))
       :ret ::read-csv-args)))

(defn guess-spec
  "This function takes a source of csv lines (either a *Reader*, *InputStream* or *String* URI)
  and tries to guess the specs necessary to parse it. You can use the option map to specify some values
  for the spec. Recognised options are:

  *Analysis options*

  +  **:header?**: Whether the file has a header on the first line
  +  **:sample-size**: number of lines on which heuristics are applied. Defaults to *100*
  +  **:guess-types?**: Whether to try to guess types for each field. Defaults to *true*
  +  **:skip**: Number of lines to skip before starting to parse. Defaults to 0

  *Schema options*

  In  a **:fields** key, the seq of fields specs you want to override in the inference process.
  Mostly used to specify manually name of column or type for special needs. The same effect cab be had by editing the resulting spec.
  Fields specs are maps with the following keys:

  + **:field**: The name of the column in the results
  + **:type**: the type to use for coercing the value. Can be one of (:float :percent :double :long :integer :string)

  A *nil* field spec means to skip this column in the results.
  An empty map means full inference for this field.
  A *Keyword* or *String* instead of a map simply defines the name of the column in the results.

  Example:
  ```clojure
  {:fields [{:type :string :field :my-field-name} nil {} {:type :float :field :my-float-field}]}
  ```

  *File options*

  +  **:encoding**: Character encoding for the file

  *Processing options*

  +  **:field-names-fn**: fn to apply to the name of each field. Defaults to trim function

  *Format options*

  +  **:delimiter**: Character used as a delimiter"
  ([uri
    {:keys [header? fields field-names-fn encoding
            guess-types? delimiter
            sample-size bom skip]
     :or {guess-types? true
          sample-size 100
          skip 0}
     :as opts}]
   (let [norm-opts (normalize-csv-options opts)
         fields-seq (:fields norm-opts)
         [^BufferedReader rdr clean-rdr enc bom-name] (get-reader uri encoding bom)]
     (try
       (when (and skip (> skip 0)) (dotimes [_ skip] (.readLine rdr)))
       (let [{guessed-schema :fields
              guessed-delimiter :delimiter
              guessed-header :header?
              guessed-quote :quoted?
              :as analysis} (try
                              (analyze-csv rdr sample-size)
                              (catch Exception e
                                (throw e)
                                {}))
             given-field-names? (some boolean (map :field (:fields norm-opts)))
             vec-output? (or (and (not given-field-names?) (false? header?))
                             (and (not given-field-names?) (not guessed-header) (not header?)))
             fnames (when-not vec-output?
                      (if (and fields-seq (every? #(or (:field %) (nil? %)) fields-seq))
                        (map :field fields-seq)
                        (when (or header? guessed-header given-field-names?)
                          (let [raw-headers (:possible-header analysis)
                                fname-fn (cond
                                           field-names-fn field-names-fn
                                           (false? header?) (if (every? keyword?
                                                                        (remove nil?
                                                                                (map :field fields-seq)))
                                                              keyword
                                                              identity)
                                           (not (some (fn [col]
                                                        (if (nil? col)
                                                          false
                                                          (re-find #"[\s':\\/@\(\)]" col))) raw-headers)) keyword
                                           :else identity)]
                            (for [[idx raw-header] (map-indexed (fn [idx v] [idx v]) raw-headers)
                                  :let [trimmed-header (if (seq raw-header)
                                                         (str/trim raw-header)
                                                         ::null)
                                        given-field (nth fields-seq idx ::not-found)
                                        given-field-label (:field given-field ::not-found)]]
                              (cond
                                (and given-field
                                     (not (or (map? given-field) (= ::not-found given-field))))
                                (:field given-field)

                                (and (or (= trimmed-header ::null) (false? header?))
                                     (= given-field-label ::not-found)) (fname-fn (str "col" idx))

                                trimmed-header (fname-fn trimmed-header)
                                (= given-field ::not-found) nil))))))
             full-schema (for [idx (range (max ((fnil count []) (:fields norm-opts))
                                               ((fnil count []) fnames)
                                               ((fnil count []) guessed-schema)))]
                           (let [guessed-type (nth guessed-schema idx ::not-found)
                                 given-field (nth fields-seq idx ::not-found)
                                 label (nth fnames idx ::not-found)]
                             (if (or (nil? label) (nil? given-field))
                               nil
                               (-> {}
                                   (cond-> (not= label ::not-found) (assoc :field label))
                                   (cond-> (and guess-types? (not= guessed-type ::not-found))
                                     (merge guessed-type))
                                   (cond-> (not= given-field ::not-found) (merge given-field))
                                   (update :type #(if (keyword? %) % :string))))))]
         {:fields (into [] full-schema) :delimiter (or delimiter guessed-delimiter) :bom bom-name
          :encoding enc :skip-analysis? true :header? (if (nil? header?) guessed-header header?)
          :quoted? guessed-quote})
       (finally
         (clean-rdr rdr)))))
  ([uri] (guess-spec uri {})))

(defn ^:no-doc parse-csv
  [^java.io.Reader rdr clean-rdr csv-opts]
  #?(:bb
     (let [row-seq (apply csv/read-csv rdr :separator (:delimiter csv-opts)
                          (if (:quoted? csv-opts) [:quote \"] []))
           vec-output? (not (every? (fn [f] (if (map? f) (:field f) true)) (:fields csv-opts)))
           ks (into [] (map :field (:fields csv-opts)))]
       (->> row-seq
            (drop (if (:header? csv-opts) 1 0))
            (map #(row->clj % csv-opts))))
     :clj
     (let [csv-rdr (make-csv-reader csv-opts)
           lazy-parse-csv (fn lazy-parse-csv [^CsvParser parser]
                            (lazy-seq
                             (when-let [row (.nextRow parser)]
                               (cons (row->clj row csv-opts) (lazy-parse-csv parser)))))]
       (lazy-parse-csv (.parse csv-rdr rdr)))))

#?(:bb nil
   :clj
   (s/fdef read-csv
     :args (s/cat :input ::csv-source
                  :options (s/? ::read-csv-args))))

(defn read-csv
  "This function takes a source of csv lines (either a *Reader*, *InputStream* or *String* URI)
  and returns the parsed results. If headers are found on the file or field names where given
  as options, it will return a collection of one map per line, associating each field name with
  its value. If not, one vector will be returned for each line, in order.

 You can use the option map to specify some values for the spec. Recognised options are:

 *Analysis options*

 +  **header?**: Whether the file has a header on the first line
 +  **sample-size**: number of lines on which heuristics are applied. Defaults to *100*
 +  **guess-types?**: Whether to try to guess types for each field. Defaults to *true*
 +  **skip-analysis?**: Whether to completely bypass analysis and only use spec
    Defaults to *false*
 +  **skip**: Number of lines to skip before starting to parse. Defaults to 0
 +  **null**: String or set of strings to be coerced to nil

 *Schema options*

  In  a **:fields** key, the seq of fields specs you want to override in the inference process.
  Mostly used to specify manually name of column or type for special needs.
  Fields specs are maps with the following keys:

  + **:field**: The name of the column in the results
  + **:type**: the type to use for coercing the value. Can be one of :float :percent :double :long :integer :string
  + **:preprocess-fn**: function applied to value as strings, before type coercion. Defaults to (fn [^String v] str/trim)
  + **:postprocess-fn**: function applied to values after type coercion. Defaults to *identity*

  A *nil* field spec means to skip this column in the results.
  An empty map means full inference for this field.
  A *Keyword* or *String* instead of a map simply defines the name of the column in the results.

  Example:
  ```clojure
  {:fields [{:type :string :field :my-field-name} nil {} {:type :float :field :my-float-field}]}
  ```

 *Processing options*

 +  **field-names-fn**: fn to apply to the name of each field. Can be used to sanitize header
    names. Defaults to trim function

 *Format options*

 +  **delimiter**: Character used as a delimiter"
  ([uri
    {:keys [header? field-names-fn fields encoding
            guess-types?
            skip-analysis?
            bom skip null]
     :or {guess-types? true
          skip 0}
     :as opts}]
     (let [{:keys [header? fields field-names-fn encoding
                   guess-types? greedy?
                   limit skip-analysis? bom]
            :or {guess-types? true
                 field-names-fn str/trim}
            :as full-spec} (normalize-csv-options
                            (if skip-analysis?
                              opts
                              (do
                                (when-not (or (instance? String uri)
                                              (instance? Reader uri)
                                              (instance? java.net.URL uri)
                                              (instance? java.io.File uri))
                                  (throw (ex-info "Cannot guess the specs of inputs that are neither String path nor File" {:uri uri})))
                                (guess-spec uri opts))))
            [^BufferedReader rdr clean-rdr _] (get-reader uri encoding bom)]
       (try
         (let [given-field-names? (some boolean (map :field (:fields full-spec)))
               named-fields? (or header? given-field-names?)
               csv-opts (merge opts full-spec {:named-fields? named-fields?})
               _ (when (and skip (> skip 0)) (dotimes [_ skip] (.readLine rdr)))
               csv-seq (parse-csv rdr clean-rdr csv-opts)]
           csv-seq)
         (catch Exception e
           (clean-rdr rdr)
           (throw e)))))
  ([uri] (read-csv uri {})))

;; (defn write-csv
;;   [input data {:keys [encoding fields delimiter headers?]
;;                :or {encoding "utf-8"
;;                     delimiter \;}
;;                :as opts}]
;;   (let [norm-fields (normalize-fields-schema fields)
;;         named-fields? (map? (first fields))
;;         given-header-fields (when (every? :field norm-fields)
;;                               (map :field norm-fields))
;;         extracted-header-fields (when (map? fields)
;;                                   (sort-by :field (map (fn [k v] (assoc v :field k)) fields)))
;;         actual-headers (or given-header-fields extracted-header-fields)
;;         [writer clean-writer] (if (instance? java.io.Writer input)
;;                                 [input (constantly true)]
;;                                 (let [wrt (io/writer input :encoding encoding)]
;;                                   [wrt #(.close wrt)]))

;;         with-headers? (if (not (nil? headers?))
;;                         headers?
;;                         (boolean actual-headers))
;;         process-row (if named-fields?
;;                         (fn [r]
;;                           (reduce (fn [acc {:keys [field key]}]
;;                                     ())
;;                                   (transient {}) )))
;;         csv-writer (CsvWriter.)]
;;     (.setFieldSeparator csv-writer delimiter)
;;     (let [appender (.append csv-writer writer)]
;;       (when with-headers?
;;         (.appendLine appender (map name actual-headers)))
;;       (doseq [line data]
;;         (.appendLine appender data)))))
