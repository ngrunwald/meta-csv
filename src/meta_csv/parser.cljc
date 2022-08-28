(ns meta-csv.parser
  (:import [java.io PushbackReader Reader]))

(defn next-char?
  ([^Character test ^PushbackReader rdr {:keys [pure]}]
   (let [ci (.read rdr)]
     (when (>= ci 0)
       (let [ca (Character/toChars ci)
             res (when (= (alength ca) 1)
                     (= test (aget ca 0)))]
         (if res
           (do (when pure (.unread rdr ca)) res)
           (do (.unread rdr ca) res)))))))

(def quote-next? (fn [c] (next-char? \" c nil)))

(defn read-record [^Character delimiter ^PushbackReader rdr & {:keys [ignore-quotes? analysis]}]
  (loop [inside-quote? false
         current-field (StringBuffer.)
         record (transient [])
         report {}]
    (let [ci (.read rdr)]
      (if (>= ci 0)
        (let [ca (Character/toChars ci)]
          (if (> (alength ca) 1)
            (recur inside-quote? (.append current-field ca) record report)
            (let [c (aget ca 0)]
              (cond
                (and (= c \") (not ignore-quotes?))
                (cond
                  (and inside-quote? (quote-next? rdr)) (recur inside-quote? (.append current-field c) record report)
                  (and inside-quote? (next-char? delimiter rdr nil)) (let [field (.toString current-field)] (recur false (StringBuffer.) (conj! record field) report))
                  (and inside-quote? (or (next-char? \newline rdr nil) (next-char? \return rdr nil))) (if (and analysis (= 0 (count record)))
                                                                                                        nil
                                                                                                        (do
                                                                                                          (when (= c \return)
                                                                                                            (next-char? \newline rdr nil))
                                                                                                          (let [field (.toString current-field)]
                                                                                                            (persistent! (conj! record field)))))
                  (and (not inside-quote?) (= (.length current-field) 0)) (recur true current-field record (assoc report :quoted? true))
                  :else (throw (ex-info "Single quote in quoted field or space at the exterior end of quoted field" {})))

                (= c delimiter) (if inside-quote?
                                  (recur inside-quote? (.append current-field ca) record report)
                                  (let [field (.toString current-field)]
                                    (recur false (StringBuffer.) (conj! record field) report)))

                (and (not inside-quote?) (or (= c \newline) (= c \return)))
                ;; drop the \n if DOS style eol
                (if (and analysis (= 0 (count record)))
                  nil
                  (do
                    (when (= c \return)
                      (next-char? \newline rdr nil))
                    (let [field (.toString current-field)]
                      (persistent! (conj! record field)))))

                :else (recur inside-quote? (.append current-field ca) record report)))))
        (if (and (= (.length current-field) 0) (= 0 (count record)))
          nil
          (let [field (.toString current-field)]
            (with-meta (persistent! (conj! record field)) report)))))))

(defn ^:no-doc lazy-read* [delimiter input options]
  (when-let [r (read-record delimiter ^Reader input options)]
    (cons r (lazy-seq (lazy-read* delimiter input options)))))

(defn lazy-read [^Reader input delimiter options]
  (let [rdr (PushbackReader. input 2)]
    (lazy-read* delimiter rdr options)))

(defn greedy-read [^Reader input delimiter options]
  (let [rdr (PushbackReader. input 2)]
    (loop [res (transient [])]
      (if-let [r (read-record delimiter rdr options)]
        (recur (conj! res r))
        (persistent! res)))))
