(ns meta-csv.core-test
  (:require [clojure.test :refer :all]
            [meta-csv.core :refer :all]
            [testit.core :refer :all]
            [clojure.string :as str]))

(deftest guess-spec-test
  (let [marine-spec (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv")
        card-spec (guess-spec "./dev-resources/samples/electronic-card-transactions.csv")
        marine-spec-no-types (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv"
                                         {:guess-types? false})]
    (facts "guess-spec auto"

           marine-spec =>
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
            :header? true,
            :quoted? false}

           card-spec =>
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
            :header? true,
            :quoted? false})

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
                       {:skip 1}) =in=>
           {:header? false})

    )

  (let [marine-spec (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv" {:header? false, :skip 1})]
    (fact "guess-spec auto without fieldnames"
          marine-spec => {:fields
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
                          :header? false,
                          :quoted? false})))
