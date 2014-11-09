-module(world).
-include("world.hrl").

%% API
-export([countries/0, countries/1, scripts/0, currencies/0, currencies/1, languages/0]).
-export([language/1, script/1, country/1, country/2, currency/1, currency/2]).

-type alive_state_option() :: live | historic.



-type language_lookup() :: {terminology | bibliographic | alpha2, string() | binary()}.

-spec languages() -> {ok, [language()]}.
languages() ->
  {ok, tuple_to_list(world_data:languages_data())}.

-spec language(language_lookup() | binary2() | string()) -> {ok, language()} | undefined.
language({Key, Term}) when is_list(Term) ->
  language({Key, list_to_binary(Term)});
language({Key, Val}) when Key == terminology; Key == bibliographic; Key == alpha2 ->
  case world_data:languages_index({Key, Val}) of
    undefined -> undefined;
    Idx -> {ok, element(Idx, world_data:languages_data())}
  end;
language(Alpha) when is_list(Alpha); is_binary(Alpha) ->
  language({alpha2, Alpha}).



-type script_lookup() :: {numeric, numeric3() | string()}  | {alpha4, binary4() | string()}.

-spec scripts() -> {ok, [script()]}.
scripts() ->
  {ok, tuple_to_list(world_data:scripts_data())}.

-spec script(script_lookup()) -> {ok, script()} | undefined.
script({Key, Val}) when is_list(Val) ->
  script({Key, list_to_binary(Val)});
script({Key, Val}) when Key == numeric; Key == alpha4 ->
  case world_data:scripts_index({Key, Val}) of
    undefined -> undefined;
    Idx -> {ok, element(Idx, world_data:scripts_data())}
  end.


-type country_lookup() :: {numeric, numeric3() | string()} | {alpha2, binary2() | string()} | {alpha3, binary3() | string()}.
-type mix_countries() :: {ok, [country()]} | {ok, [historic_country()]} | {ok, [country()], [historic_country()]}.
-type maybe_mix_country() :: {ok, country() | historic_country()} | undefined.

-spec countries() -> {ok, [country()]}.
countries() ->
  countries([]).


-spec countries([alive_state_option()]) -> mix_countries().
countries(Options) when is_list(Options) ->
  case {lists:member(historic, Options), lists:member(live, Options)} of
    {true, true} ->
      {ok, tuple_to_list(world_data:countries_data()), tuple_to_list(world_data:countries_3_data())};
    {true, false} ->
      {ok, tuple_to_list(world_data:countries_3_data())};
    _ ->
      {ok, tuple_to_list(world_data:countries_data())}
  end.


-spec country(country_lookup() | string() | binary2(), [alive_state_option()]) -> maybe_mix_country().
country({Key, Val}, Options) when is_list(Val) ->
  country({Key, list_to_binary(Val)}, Options);
country({Key, Val}, Options) when Key == numeric; Key == alpha2; Key == alpha3 ->
  case {lists:member(historic, Options), lists:member(live, Options)} of
    {true, true} ->
      case world_data:countries_index({Key, Val}) of
        undefined ->
          case world_data:countries_3_index({Key, Val}) of
            undefined -> undefined;
            Idx -> {ok, element(Idx, world_data:countries_3_data())}
          end;
        Idx ->
          {ok, element(Idx, world_data:countries_data())}
      end;
    {true, false} ->
      case world_data:countries_3_index({Key, Val}) of
        undefined -> undefined;
        Idx -> {ok, element(Idx, world_data:countries_3_data())}
      end;
    _ ->
      case world_data:countries_index({Key, Val}) of
        undefined -> undefined;
        Idx -> {ok, element(Idx, world_data:countries_data())}
      end
  end;
country(Alpha, Options) when is_list(Alpha); is_binary(Alpha) ->
  country({alpha2, Alpha}, Options).

-spec country(country_lookup() | string() | binary2()) -> maybe_mix_country().
country({Key, Val}) when is_list(Val) ->
  country({Key, list_to_binary(Val)});
country(Alpha) when is_list(Alpha) ->
  country(list_to_binary(Alpha));
country(Alpha) when is_binary(Alpha) ->
  country({alpha2, Alpha});
country({Key, Val}) ->
  country({Key, Val}, []).


-type currency_lookup() :: {letter, binary3() | string()} | {numeric, numeric3() | string()}.
-type mix_currencies() :: {ok, [currency()]} | {ok, [historic_currency()]} | {ok, [currency()], [historic_currency()]}.
-type maybe_mix_currency() :: {ok, currency() | historic_currency()} | undefined.

-spec currencies() -> {ok, [currency()]}.
currencies() ->
  currencies([]).

-spec currencies([alive_state_option()]) -> mix_currencies().
currencies(Options) when is_list(Options) ->
  case {lists:member(historic, Options), lists:member(live, Options)} of
    {true, true} ->
      {ok, tuple_to_list(world_data:currencies_data()), tuple_to_list(world_data:historic_currencies_data())};
    {true, false} ->
      {ok, tuple_to_list(world_data:historic_currencies_data())};
    _ ->
      {ok, tuple_to_list(world_data:currencies_data())}
  end.

-spec currency(currency_lookup() | string() | binary3(), [alive_state_option()]) -> maybe_mix_currency().
currency({Key, Val}, Options) when is_list(Val) ->
  currency({Key, list_to_binary(Val)}, Options);
currency({Key, Val}, Options) when Key == letter; Key == numeric; Key == alpha3 ->
  case {lists:member(historic, Options), lists:member(live, Options)} of
    {true, true} ->
      case world_data:currencies_index({Key, Val}) of
        undefined ->
          case world_data:historic_currencies_index({Key, Val}) of
            undefined -> undefined;
            Idx -> {ok, element(Idx, world_data:historic_currencies_data())}
          end;
        Idx ->
          {ok, element(Idx, world_data:currencies_data())}
      end;
    {true, false} ->
      case world_data:historic_currencies_index({Key, Val}) of
        undefined -> undefined;
        Idx -> {ok, element(Idx, world_data:historic_currencies_data())}
      end;
    _ ->
      case world_data:currencies_index({Key, Val}) of
        undefined -> undefined;
        Idx -> {ok, element(Idx, world_data:currencies_data())}
      end
  end;
currency(Letter, Options) when is_list(Letter); is_binary(Letter) ->
  currency({letter, Letter}, Options).

-spec currency(currency_lookup() | string() | binary3()) -> maybe_mix_currency().
currency({Key, Val}) when is_list(Val) ->
  currency({Key, list_to_binary(Val)});
currency(Letter) when is_list(Letter) ->
  currency(list_to_binary(Letter));
currency(Letter) when is_binary(Letter) ->
  currency({letter, Letter});
currency({Key, Val}) ->
  currency({Key, Val}, []).

