(ns meta-csv.core-test
  (:require [clojure.test :refer [deftest]]
            [meta-csv.core :refer [guess-spec read-csv read-csv-file]]
            [testit.core :refer [facts fact => =in=> ...]]
            [clojure.string :as str]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument)

(deftest guess-spec-test
  (let [marine-spec (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv")
        card-spec (guess-spec "./dev-resources/samples/electronic-card-transactions.csv")
        marine-spec-no-types (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv"
                                         {:guess-types? false})]
    (facts "guess-spec auto"

           marine-spec =in=>
           {:fields
            [{:field :year, :type :long}
             {:field :category, :type :string}
             {:field :variable, :type :string}
             {:field :units, :type :string}
             {:field :magnitude, :type :string}
             {:field :source, :type :string}
             {:field :data_value, :type :double}
             {:field :flag, :type :string}],
            :delimiter \,,
            :bom :none,
            :encoding "ISO-8859-1",
            :skip-analysis? true,
            :header? true,}

           card-spec =in=>
           {:fields
            [{:field :Series_reference, :type :string}
             {:field :Period, :type :double}
             {:field :Data_value, :type :string}
             {:field :Suppressed, :type :string}
             {:field :STATUS, :type :string}
             {:field :UNITS, :type :string}
             {:field :Magnitude, :type :long}
             {:field :Subject, :type :string}
             {:field :Group, :type :string}
             {:field :Series_title_1, :type :string}
             {:field :Series_title_2, :type :string}
             {:field :Series_title_3, :type :string}
             {:field :Series_title_4, :type :string}
             {:field :Series_title_5, :type :string}],
            :delimiter \,,
            :bom :none,
            :encoding "ISO-8859-1",
            :skip-analysis? true,
            :header? true,})

    (facts "override some specs"
           (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv"
                       {:guess-types? false}) =in=>
           {:fields [{:field :year :type :string} ...]}

           (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv"
                       {:field-names-fn str/upper-case}) =in=>
           {:fields [{:field "YEAR" :type :long} ...]}

           (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv"
                       {:delimiter \;}) =in=>
           {:delimiter \;}

           (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv"
                       {:skip-lines 1}) =in=>
           {:header? false}

           (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv"
                       {:fields [{:type :float} nil {:skip? true} nil nil :src {} nil]}) =in=>
           {:fields [{:field :year, :type :float}
                     nil
                     {:skip? true}
                     nil
                     nil
                     {:field :src, :type :string}
                     {:field :data_value, :type :double}
                     nil]}))

  (let [marine-spec (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv" {:header? false, :skip-lines 1})]
    (fact "guess-spec auto without fieldnames"
          marine-spec =in=> {:fields
                             [{:type :long}
                              {:type :string}
                              {:type :string}
                              {:type :string}
                              {:type :string}
                              {:type :string}
                              {:type :double}
                              {:type :string}],
                             :delimiter \,,
                             :bom :none,
                             :encoding "ISO-8859-1",
                             :skip-analysis? true,
                             :header? false})))

(deftest read-csv-test


  (let [results (read-csv "./dev-resources/samples/marine-economy-2007-18.csv"
                          {:skip-lines 1 :header? false})]
    (facts "results as array"
           results =in=> [[2007
                           "Fisheries and aquaculture"
                           "Cont. to ME Wage and salary earners"
                           "Proportion"
                           "Actual"
                           "LEED"
                           43.1
                           "R"] ...]

           results => seq?))

  (fact "greedy reading"
    (read-csv "./dev-resources/samples/marine-economy-2007-18.csv" {:greedy? true}) => vector?)

  (let [results (read-csv "./dev-resources/samples/marine-economy-2007-18.csv")]
    (facts "base case"
           results =in=> [{:year 2007,
                           :category "Fisheries and aquaculture",
                           :variable "Cont. to ME Wage and salary earners",
                           :units "Proportion",
                           :magnitude "Actual",
                           :source "LEED",
                           :data_value 43.1,
                           :flag "R"} ...]

           results => seq?

           (first results) => #(instance? clojure.lang.PersistentArrayMap %)))

  (fact "test array-map"
        (read-csv "./dev-resources/samples/electronic-card-transactions.csv") =>
        (comp #(instance? clojure.lang.PersistentArrayMap %) last))

  (fact "reading with overrides"
        (read-csv "./dev-resources/samples/marine-economy-2007-18.csv"
                  {:fields [{:type :float} nil {:skip? true} nil nil :src {} nil]}) =in=>
        [{:year 2007.0, :src "LEED", :data_value 43.1} ...])

  (fact "using process-fn"
        (read-csv "./dev-resources/samples/marine-economy-2007-18.csv"
                  {:fields [{} nil nil nil nil nil {:field :target :preprocess-fn #(str "1" %)
                                                    :postprocess-fn inc} nil]})
        =in=>
        [{:year 2007 :target 144.1} ...])

  (fact "with field-names-fn"
        (read-csv "./dev-resources/samples/marine-economy-2007-18.csv"
                  {:field-names-fn str/upper-case}) =in=>
        [{"YEAR" 2007} ...])

  (fact "all strings no guess"
        (read-csv "./dev-resources/samples/marine-economy-2007-18.csv"
                  {:guess-types? false}) =in=>
        [{:year "2007"} ...])

  (facts "null value"
         (read-csv "./dev-resources/samples/marine-economy-2007-18.csv"
                   {:null "2007"}) =in=> [{:year nil} ...]
         (read-csv "./dev-resources/samples/marine-economy-2007-18.csv"
                   {:null #{"2007"}}) =in=> [{:year nil} ...])

  (fact "tricky csv with lots of padding and quotes"
    (read-csv-file "./dev-resources/samples/tricky.csv") =in=> [["str1" "str2" "num" "str3"]
                                                                ["foo" "bar" "42" "baz"]
                                                                ["foo" "bar" "42" "baz"]
                                                                [" foo" " \"bar \"" "42" "baz"]
                                                                [" foo" " \"bar \"" "42" "baz"]
                                                                [" foo " " \"bar \"" "42" "baz"]
                                                                ["foo " " \" bar \" " "42" "baz"]
                                                                ...]))
