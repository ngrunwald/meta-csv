(ns meta-csv.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [meta-csv.parser :as parser]
            [medley.core :as med])
  (:import [java.io Reader BufferedReader File StringReader]
           [java.util Collection]
           #?@(:bb []
               :clj [[com.ibm.icu.text CharsetDetector]
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
             #?@(:bb [:coercer #(Double/parseDouble %)]
                 :clj [:coercer #(Float/parseFloat %)])}
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
  [f]
  #?(:bb "UTF-8" ;; use file -bi later if present to try a guess
     :clj
     (with-open [is (io/input-stream f)]
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
           "UTF-8")))))

(def ^:no-doc boms
  {[(byte -1) (byte -2)] [:utf16-le 2]
   [(byte -2) (byte -1)] [:utf16-be 2]
   [(byte -17) (byte -69) (byte -65)] [:utf8 3]
   [(byte -1) (byte -2) (byte 0) (byte 0)] [:utf32-le 4]
   [(byte 0) (byte 0) (byte -2) (byte -1)] [:utf32-be 4]})

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

(defn ^:no-doc guess-bom
  [f]
  (with-open [stream (io/input-stream f)]
    (let [bom (byte-array 4)]
      (.read stream bom)
      (let [[a b c _ :as first-four] (into [] (seq bom))
            first-two [a b]
            first-three [a b c]
            [bom-name bom-size] (or (boms first-two) (boms first-three) (boms first-four) [:none 0])]
        bom-name))))

(defn analyze-file [f]
  (let [enc (guess-charset f)
        bom-name (guess-bom f)]
    {:encoding enc :bom bom-name :path f}))

(defn file-reader
  [{:keys [encoding bom path skip-lines]
    :or {skip-lines 0}}]
  (let [istream (if (and bom (not= bom :none))
                  (let [bom-size (bom-sizes bom)
                        stream (io/input-stream path)]
                    (.read stream (byte-array bom-size))
                    stream)
                  (io/input-stream path))
        ^BufferedReader rdr (io/reader istream :encoding encoding)]
    (dotimes [_ skip-lines]
      (.readLine rdr))
    rdr))

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

(defn ^:no-doc find-char-pos
  [^String raw-record char]
  (loop [found []
         cur 0]
    (let [pos (.indexOf raw-record (int char) cur)]
      (if (not= pos -1)
        (recur (conj found pos) (inc pos))
        found))))

;; #?(:bb
;;    nil
;;    :clj
;;    (defn ^:no-doc make-csv-reader
;;      ^CsvReader [{:keys [delimiter skip-empty-rows? header?
;;                          text-delimiter error-on-different-field-count?]}]
;;      (let [^CsvReader reader (CsvReader.)]
;;        (when delimiter (.setFieldSeparator reader delimiter))
;;        (when skip-empty-rows? (.setSkipEmptyRows reader true))
;;        (when header? (.setContainsHeader reader true))
;;        (when text-delimiter (.setTextDelimiter reader text-delimiter))
;;        (when error-on-different-field-count? (.setErrorOnDifferentFieldCount reader true))
;;        reader)))

(defn ^:no-doc row->clj
  [row
   {:keys [fields field-names-fn named-fields? null]}]
  (let [{:keys [quoted-fields] :as record} (meta row)]
    (try
      (loop [todo fields
             row-idx 0
             idx 0
             acc (transient [])]
        (if (seq todo)
          (let [{:keys [field type skip? postprocess-fn preprocess-fn]
                 :or {postprocess-fn default-postprocess-fn
                      preprocess-fn (if (quoted-fields row-idx) identity default-preprocess-fn)} :as spec} (first todo)]
            (if (or skip? (nil? spec))
              (recur (rest todo) (inc row-idx) idx acc)
              (let [kname (if named-fields? (field-names-fn field) idx)
                    ^String v (row row-idx)
                    preproc-v (preprocess-fn v)
                    ^String filter-v (if (and null preproc-v
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
            {})))
      (catch Exception e
        (let [line-number nil]
          (throw (ex-info (str "Error reading row " line-number)
                          {:line-number line-number}
                          e)))))))

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

(defn ^:no-doc sample-records
  [input delimiters-set options]
  (let [new-reader-fn (if (map? input)
                        file-reader
                        (fn [i] (StringReader. i)))]
    (loop [delimiters delimiters-set
           candidates nil]
      (if-let [delim (first delimiters)]
        (let [^Reader rdr (new-reader-fn input)
              records (try (parser/lazy-read rdr delim (assoc options :analysis true)) (catch Exception _ nil))
              sample (doall (take 100 records))
              fields-count (map count sample)]
          (.close rdr)
          (if (seq sample)
            (recur (rest delimiters)
                   (conj candidates {:sample sample :delimiter delim
                                     :fields-max-extent (apply max fields-count)
                                     :fields-min-extent (apply min fields-count)}))
            (recur (rest delimiters) candidates)))
        (first
         (sort-by (fn [{:keys [fields-max-extent fields-min-extent]}]
                    [(- fields-min-extent fields-max-extent) fields-min-extent])
                  > candidates))))))

(defn ^:no-doc analyze-csv
  [input]
  (let [{:keys [delimiter sample]} (sample-records input #{\, \; \tab} {})
        fields-schema (guess-types (drop 1 sample))
        has-header? (is-header? (first sample) fields-schema)]
    {:delimiter delimiter :fields-schema fields-schema :header? has-header?
     :possible-header (first sample)}))

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
  :ret ::read-csv-args)

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
  ([path
    {:keys [header? fields field-names-fn encoding
            guess-types? delimiter
            sample-size bom skip-lines ignore-quotes?]
     :or   {guess-types? true
            sample-size  100
            skip-lines   0}
     :as   opts}]
   (let [norm-opts         (normalize-csv-options opts)
         fields-seq         (:fields norm-opts)
         file-data          (med/assoc-some (analyze-file path)
                                            :encoding encoding
                                            :bom (when bom (keyword bom))
                                            :skip-lines skip-lines)
         {guessed-schema    :fields-schema
          guessed-delimiter :delimiter
          guessed-header    :header?
          :as               analysis}     (analyze-csv file-data)
         given-field-names? (some boolean (map :field (:fields norm-opts)))
         vec-output? (or (and (not given-field-names?) (false? header?))
                         (and (not given-field-names?) (not guessed-header) (not header?)))
         fnames (when-not vec-output?
                  (if (and fields-seq (every? #(or (:field %) (nil? %)) fields-seq))
                    (map :field fields-seq)
                    (when (or header? guessed-header given-field-names?)
                      (let [raw-headers (:possible-header analysis)
                            fname-fn    (cond
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
                                          :else                                                          identity)]
                        (for [[idx raw-header] (map-indexed (fn [idx v] [idx v]) raw-headers)
                              :let             [trimmed-header (if (seq raw-header)
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

                            trimmed-header              (fname-fn trimmed-header)
                            (= given-field ::not-found) nil))))))
         full-schema (for [idx (range (max ((fnil count []) (:fields norm-opts))
                                           ((fnil count []) fnames)
                                           ((fnil count []) guessed-schema)))]
                       (let [guessed-type (nth guessed-schema idx ::not-found)
                             given-field  (nth fields-seq idx ::not-found)
                             label        (nth fnames idx ::not-found)]
                         (if (or (nil? label) (nil? given-field))
                           nil
                           (-> {}
                               (cond-> (not= label ::not-found) (assoc :field label))
                               (cond-> (and guess-types? (not= guessed-type ::not-found))
                                 (merge guessed-type))
                               (cond-> (not= given-field ::not-found) (merge given-field))
                               (update :type #(if (keyword? %) % :string))))))]
     (merge file-data
            {:fields         (into [] full-schema)
             :delimiter      (or delimiter guessed-delimiter)
             :skip-analysis? true
             :header?        (if (nil? header?) guessed-header header?)
             :ignore-quotes? ignore-quotes?})))
  ([path] (guess-spec path {})))

;; (defn ^:no-doc parse-csv
;;   [^java.io.Reader rdr clean-rdr csv-opts]
;;   #?(:bb
;;      (let [row-seq (apply csv/read-csv rdr :separator (:delimiter csv-opts)
;;                           (if (:quoted? csv-opts) [:quote \"] []))
;;            vec-output? (not (every? (fn [f] (if (map? f) (:field f) true)) (:fields csv-opts)))
;;            ks (into [] (map :field (:fields csv-opts)))]
;;        (->> row-seq
;;             (drop (if (:header? csv-opts) 1 0))
;;             (map #(row->clj % csv-opts))))
;;      :clj
;;      (let [csv-rdr (make-csv-reader csv-opts)
;;            lazy-parse-csv (fn lazy-parse-csv [^CsvParser parser]
;;                             (lazy-seq
;;                              (when-let [row (.nextRow parser)]
;;                                (cons (row->clj row csv-opts) (lazy-parse-csv parser)))))]
;;        (lazy-parse-csv (.parse csv-rdr rdr)))))

#?(:bb nil
   :clj
   (s/fdef read-csv
     :args (s/cat :input ::csv-source
                  :options (s/? ::read-csv-args))))

(defn read-csv-file
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
  ([input
    {:keys [header? field-names-fn fields encoding
            guess-types? greedy?
            skip-analysis?
            bom skip-lines]
     :or {guess-types? true
          skip-lines 0}
     :as opts}]
     (let [{:keys [header? fields field-names-fn encoding
                   guess-types?
                   limit skip-analysis? bom]
            :or {guess-types? true
                 field-names-fn str/trim}
            :as full-spec} (normalize-csv-options
                            (if skip-analysis?
                              opts
                              (do
                                (when-not (or (instance? String input)
                                              (instance? java.io.File input))
                                  (throw (ex-info "Cannot guess the specs of inputs that are neither String path nor File"
                                                  {:input input})))
                                (guess-spec input opts))))
           rdr (file-reader {:path (str input) :encoding encoding :bom bom :skip-lines skip-lines})
           given-field-names? (some boolean (map :field (:fields full-spec)))
           named-fields? (or header? given-field-names?)
           csv-opts (merge opts full-spec {:named-fields? named-fields?})
           starting-line (+ skip-lines (if header? 1 0))
           read-fn (if greedy? parser/greedy-read parser/lazy-read)
           map-fn (if greedy? mapv map)
           raw-records (read-fn rdr (:delimiter csv-opts) (assoc csv-opts :line-number starting-line))]
       (map-fn (fn [row] (let [{:keys [starting-line]} (meta row)]
                           (vary-meta (row->clj row csv-opts) assoc :starting-line starting-line)))
            (if (:header? csv-opts) (rest raw-records) raw-records))))
  ([input] (read-csv-file input {})))

(def ^:deprecated read-csv read-csv-file)

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
