# meta-csv

A smart reader for CSV files, spiritual successor to [ultra-csv](https://github.com/ngrunwald/ultra-csv).

## Features

  - Smart statistical heuristics to guess pretty much anything about
  your csv file, from delimiter to quotes and whether a header is present
  - Handles for you the boring but dangerous stuff, like encoding detection
  and bom skipping if present, but also embedded new lines and quote escaping
  - Coerces the numerical values that have been recognised. The types and
  coercions can be extended by the user to *dates*, *phone numbers*, etc.
  - Designed to be both very easy to use in an exploratory way to get a quick
  feel for the data, and then be put into production with almost the same code

## Installation

`meta-csv` is available as a Maven artifact from
[Clojars](http://clojars.org/meta-csv):

In your `project.clj` dependencies for *leiningen*:

[![Clojars Project](http://clojars.org/meta-csv/latest-version.svg)](http://clojars.org/meta-csv)

## Usage

The easiest way to use when hacking at the REPL is simply:

```clojure
(require '[meta-csv.core :as csv])
(def csv-file-path "dev-resources/samples/marine-economy-2007-18.csv")
```

```clojure
(first (csv/read-csv csv-file-path))

=> {:year 2007,
    :category "Fisheries and aquaculture",
    :variable "Cont. to ME Wage and salary earners",
    :units "Proportion",
    :magnitude "Actual",
    :source "LEED",
    :data_value 43.1,
    :flag "R"}
```

If the file has a header, this returns a lazy seq of maps of field names to values.

If any field name would be problematic as keyword, then all field names will be
strings instead:

```clojure
(first (csv/read-csv "./dev-resources/samples/sales-records-sample.csv"))

=> {"Region" "Australia and Oceania",
    "Country" "Tuvalu",
    "Item Type" "Baby Food",
    "Sales Channel" "Offline",
    "Order Priority" "H",
    "Order Date" "5/28/2010",
    "Order ID" 669165933,
    "Ship Date" "6/27/2010",
    "Units Sold" 9925,
    "Unit Price" 255.28,
    "Unit Cost" 159.42,
    "Total Revenue" 2533654.0,
    "Total Cost" 1582243.5,
    "Total Profit" 951410.5}
```

The maps are array-maps, which means the order of the keys is the same as the
order of the fields in the file.

If no header is present, the rows will be returned as a seq of vectors, in the same fashion as [clojure.data.csv/read-csv](https://clojure.github.io/data.csv/#clojure.data.csv/read-csv).

A lot of options are available, as an optional second argument spec. Check the
docstring for a more or less exhaustive description.

This spec can actually be created by another noteworthy function, `guess-spec`.

```clojure
(csv/guess-spec "./dev-resources/samples/marine-economy-2007-18.csv")

=> {:fields
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
```

Then the `:fields` vector describing the processing on each field can be
customized to produce exactly the right format of data for going to production.

More documentation forthcoming, for now you can use the quite exhaustive
docstrings in the [API
Documentation](https://cljdoc.org/d/meta-csv/meta-csv/CURRENT/doc/readme).

[![cljdoc badge](https://cljdoc.org/badge/meta-csv/meta-csv)](https://cljdoc.org/d/meta-csv/meta-csv/CURRENT)

## License

Copyright © 2019-2020 Nils Grunwald

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
