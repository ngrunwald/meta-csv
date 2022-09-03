(ns meta-csv.parser
  (:import [java.io PushbackReader Reader]
           [java.util Collection]))

(defn next-char?
  ([^Character test ^PushbackReader rdr {:keys [pure skip-spaces]}]
   (loop [^Collection read (transient [])]
     (let [ci (.read rdr)]
       (if (>= ci 0)
         (let [ca (Character/toChars ci)
               c (when (= (alength ca) 1) (aget ca 0))]
           (cond
             (= test c) (do (when pure (.unread rdr (char-array (conj! read c)))) true)
             (and skip-spaces (<= (count read) skip-spaces) (= \space c)) (recur (conj! read c))
             :else (do (.unread rdr (char-array (persistent! (conj! read c)))) false)))
         (do (.unread rdr (Character/toChars ci))
             (when-not (.isEmpty read) (.unread rdr (char-array read)))
             false))))))

(def quote-next? (fn [c opts] (next-char? \" c opts)))

(defn read-record [^Character delimiter ^PushbackReader rdr {:keys [max-padding ignore-quotes? analysis line-number report]}]
  (loop [inside-quote? false
         current-field (StringBuffer.)
         record (transient [])
         starting-line (or line-number 0)
         total-line-count (or line-number 0)
         report (or report {:quoted-fields #{}})]
    (let [ci (.read rdr)]
      (if (>= ci 0)
        (let [ca (Character/toChars ci)
              clength (alength ca)]
          (if (> clength 1)
            (recur inside-quote? (.append current-field ca) record starting-line total-line-count report)
            (let [c (aget ca 0)]
              (cond
                (and (= c \") (not ignore-quotes?))
                (cond
                  (and inside-quote? (quote-next? rdr nil)) (recur inside-quote? (.append current-field c) record
                                                                   starting-line total-line-count report)
                  (and inside-quote? (next-char? delimiter rdr {:skip-spaces max-padding})) (let [field (.toString current-field)
                                                                                                  idx (count record)]
                                                                                              (recur false (StringBuffer.) (conj! record field)
                                                                                                     starting-line total-line-count
                                                                                                     (update report :quoted-fields conj idx)))
                  (and inside-quote? (or (next-char? \newline rdr {:skip-spaces max-padding})
                                         (next-char? \return rdr {:skip-spaces max-padding}))) (if (and analysis (= 0 (count record)))
                                                                                                 nil
                                                                                                 (do
                                                                                                   (when (= c \return)
                                                                                                     (next-char? \newline rdr nil))
                                                                                                   (let [field (.toString current-field)
                                                                                                         idx (count record)]
                                                                                                     (with-meta (persistent! (conj! record field))
                                                                                                       (-> report
                                                                                                           (assoc :starting-line starting-line
                                                                                                                  :line-number (inc total-line-count))
                                                                                                           (update :quoted-fields conj idx))))))
                  (and (not inside-quote?) (or (= (.length current-field) 0)
                                               (re-find #"^ +$" (.toString current-field)))) (recur true (StringBuffer.) record
                                                                                                    starting-line total-line-count report)
                  :else (throw (ex-info "Single quote in quoted field or space at the exterior end of quoted field" {:line-number total-line-count})))

                (= c delimiter) (if inside-quote?
                                  (recur inside-quote? (.append current-field ca) record starting-line total-line-count report)
                                  (let [field (.toString current-field)]
                                    (recur false (StringBuffer.) (conj! record field) starting-line total-line-count report)))

                (and inside-quote? (= c \newline)) (recur inside-quote? (.append current-field ca) record
                                                          starting-line (inc total-line-count) report)
                (and (not inside-quote?) (or (= c \newline) (= c \return)))
                ;; drop the \n if DOS style eol
                (if (and analysis (= 0 (count record)))
                  nil
                  (do
                    (when (= c \return)
                      (next-char? \newline rdr nil))
                    (let [field (.toString current-field)]
                      (with-meta (persistent! (conj! record field))
                        (assoc report
                               :starting-line starting-line
                               :line-number (inc total-line-count))))))

                :else (recur inside-quote? (.append current-field ca) record starting-line total-line-count report)))))
        (if (and (= (.length current-field) 0) (= 0 (count record)))
          nil
          (let [field (.toString current-field)]
            (with-meta (persistent! (conj! record field))
              (assoc report
                     :starting-line starting-line
                     :line-number total-line-count))))))))

(defn ^:no-doc lazy-read* [delimiter ^Reader input options]
  (if-let [rec (read-record delimiter input options)]
    (cons rec (lazy-seq (lazy-read* delimiter input (merge options (meta rec)))))
    (do (.close input) nil)))

(defn lazy-read [^Reader input delimiter options]
  (let [max-padding (:max-padding options 10)
        rdr (PushbackReader. input max-padding)]
    (lazy-read* delimiter rdr (assoc options :max-padding max-padding))))

(defn greedy-read [^Reader input delimiter options]
  (let [max-padding (:max-padding options 10)
        rdr (PushbackReader. input max-padding)
        full-options (assoc options :max-padding 10)]
    (loop [res (transient [])
           report (:report full-options)]
      (if-let [r (read-record delimiter rdr (merge full-options report))]
        (recur (conj! res r) (meta r))
        (do
          (.close input)
          (persistent! res))))))
