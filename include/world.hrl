-type maybe_binary() :: undefined | binary().
-type year() :: pos_integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type maybe_date() :: undefined | {year()} | {year(), month()} | {year(), month(), day()}.
-type binary4() :: <<_:32>>.
-type binary3() :: <<_:24>>.
-type binary2() :: <<_:16>>.
-type numeric3() ::binary3().
-type maybe_numeric3() :: undefined | numeric3().
-type maybe_binary2() :: undefined | binary2().



-record(script, {
  alpha4 :: binary4(),
  numeric :: numeric3(),
  name :: binary()
}).
-record(country, {
  alpha3 :: binary3(),
  alpha2 :: binary2(),
  numeric :: numeric3(),
  name :: binary(),
  official_name :: binary()
}).

-record(historic_country, {
  alpha3 :: binary3(),
  alpha2 :: binary2(),
  numeric :: maybe_numeric3(),
  names :: binary(),
  date_withdrawn :: maybe_date()
}).

-record(language, {
  alpha2 :: maybe_binary2(),
  bibliographic :: binary(),
  terminology :: binary(),
  name :: binary()
}).

-record(currency, {
  letter :: binary3(),
  numeric :: maybe_numeric3(),
  symbol :: binary(),
  name :: binary()
}).
-record(historic_currency, {
  letter :: binary3(),
  numeric :: maybe_numeric3(),
  symbol :: binary(),
  name :: binary(),
  date_withdrawn :: maybe_date()
}).



-type script() :: #script{}.
-type country() :: #country{}.
-type historic_country() :: #historic_country{}.
-type language() :: #language{}.
-type currency() :: #currency{}.
-type historic_currency() :: #historic_currency{}.