(ns meta-csv.core-test
  (:require [clojure.test :refer :all]
            [meta-csv.core :refer :all]
            [testit.core :refer :all]))

(deftest guess-spec-test
  (let [marine-spec (guess-spec "./dev-resources/samples/marine-economy-2007-18.csv")
        card-spec (guess-spec "./dev-resources/samples/electronic-card-transactions.csv")]
    (facts "guess-spec auto"
           marine-spec => #(instance? clojure.lang.PersistentArrayMap %)
           marine-spec =>
           {:fields
            '({:field :year, :type :long}
              {:field :category, :type :string}
              {:field :variable, :type :string}
              {:field :units, :type :string}
              {:field :magnitude, :type :string}
              {:field :source, :type :string}
              {:field :data_value, :type :double}
              {:field :flag, :type :string}),
            :delimiter \,,
            :bom :none,
            :encoding "ISO-8859-1",
            :skip-analysis? true,
            :header? true,
            :quoted? false}
           card-spec => #(instance? clojure.lang.PersistentArrayMap %)
           card-spec =>
           {:fields
            '({:field :Series_reference, :type :string}
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
              {:field :Series_title_5, :type :string}),
            :delimiter \,,
            :bom :none,
            :encoding "ISO-8859-1",
            :skip-analysis? true,
            :header? true,
            :quoted? false})
)
  ;; (facts "guess-spec with positional custom fields"
  ;;        (guess-spec "./dev-resources/samples/electronic-card-transactions.csv"
  ;;                    {:fields [nil
  ;;                              {:field :Period, :type :double}
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil
  ;;                              nil]}) =in=>
  ;;        )
  )
