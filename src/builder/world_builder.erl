-module(world_builder).


-include("world.hrl").
%% API
-export([build_iolist/0, build/0]).

parse_date("unknown") ->
  undefined;
parse_date(undefined) ->
  undefined;
parse_date(Date) when is_list(Date) ->
  Parts = string:tokens(Date, "-"),
  list_to_tuple([list_to_integer(P) || P <- Parts]).

attr(Name, Attrs, Fun) ->
  case lists:keyfind(Name, 3, Attrs) of
    {_, _, _, Val} -> Fun(Val);
    _ -> undefined
  end.

fix_record(#script{alpha4 = <<"Modi">>} = Rec) ->
  Rec#script{numeric = <<"324">>};

fix_record(#country{alpha2 = Alpha2, name = Name, official_name = OffName} = Rec) ->
  if
    Alpha2 == <<"RU">> -> Rec#country{official_name = Name, name = <<"Russia">>};
    Alpha2 == <<"BO">> -> Rec#country{name = <<"Bolivia">>};
    Alpha2 == <<"CD">> -> Rec#country{name = <<"DR Congo">>};
    Alpha2 == <<"VA">> -> Rec#country{name = <<"Vatican">>};
    Alpha2 == <<"IR">> -> Rec#country{name = <<"Iran">>};
    Alpha2 == <<"KP">> -> Rec#country{name = <<"North Korea">>};
    Alpha2 == <<"KR">> -> Rec#country{name = <<"South Korea">>};
    Alpha2 == <<"LA">> -> Rec#country{name = <<"Laos">>};
    Alpha2 == <<"MK">> -> Rec#country{name = <<"Macedonia">>};
    Alpha2 == <<"FM">> -> Rec#country{name = <<"Micronesia">>};
    Alpha2 == <<"MD">> -> Rec#country{name = <<"Moldova">>};
    Alpha2 == <<"PS">> -> Rec#country{name = <<"Palestine">>};
    Alpha2 == <<"SY">> -> Rec#country{name = <<"Syria">>};
    Alpha2 == <<"TW">> -> Rec#country{name = <<"Taiwan">>};
    Alpha2 == <<"VE">> -> Rec#country{name = <<"Venezuela">>};
    OffName == undefined -> Rec#country{official_name = Name};
    true -> Rec
  end;

fix_record(#historic_country{alpha2 = Alpha} = Rec) when is_binary(Alpha), byte_size(Alpha) > 2 ->
  <<HD:2/binary, _Rest/binary>> = Alpha,
  fix_record(Rec#historic_country{alpha2 = HD});
fix_record(#historic_country{alpha2 = <<"YE">>} = Rec) ->
  Rec#historic_country{numeric = <<"886">>};
fix_record(Rec) ->
  Rec.



format_idx_case_elem([], _, Acc) ->
  Acc;
format_idx_case_elem([{Idx, Elem} | Tail], N, Acc) ->
  case element(N, Elem) of
    undefined -> format_idx_case_elem(Tail, N, Acc);
    S ->
      Format = if
                 is_binary(S) -> <<"     <<\"~s\">> -> ~B;\n">>;
                 is_integer(S) -> <<"    ~B -> ~B;\n">>;
                 true -> throw({S, <<"Neither binary nor int">>})
               end,

      Str = io_lib:format(Format, [S, Idx]),
      format_idx_case_elem(Tail, N, [Str | Acc])
  end.

format_idx_case(List, Var, N) when is_binary(Var), is_integer(N), is_list(List) ->
  io_lib:format(<<"  case ~s of\n~s    _ -> undefined\n  end">>,
    [Var, format_idx_case_elem(List, N, [])]).

format_raw_elem([], Acc) -> Acc;
format_raw_elem([{_N, Elem} | Tail], Acc) ->
  Separator = case Acc of [] -> <<>>; _ -> <<", ">> end,
  Str = io_lib:format("\n    ~p~s", [Elem, Separator]),
  format_raw_elem(Tail, [Str | Acc]).



format_body(Name, Data) ->
  [<<"\n\n">>, Name, <<"() -> \n  {">>,
    format_raw_elem(Data, []),
    <<"\n  }.">>].

format_index([], _Name, _Data, Acc) ->
  Acc;

format_index([{Key, Var, N}  | Tail], Name, Data, Acc) ->
  Delim = if Acc == [] -> <<".">>; true -> <<";">> end,
  format_index(Tail, Name, Data,
    [
     io_lib:format(
        <<"\n\n~s({~s, ~s}) ->\n~s~s\n">>, [Name, Key, Var, format_idx_case(Data, Var, N), Delim]
      )
      | Acc
    ]).

build_languages() ->
  EventFun = fun
    ({startElement, _, "iso_639_entry", _QName, Attrs}, _, {N, Acc}) ->
      Rec = #language{
        alpha2 = attr("iso_639_1_code", Attrs, fun unicode:characters_to_binary/1),
        bibliographic = attr("iso_639_2B_code", Attrs, fun unicode:characters_to_binary/1),
        terminology = attr("iso_639_2T_code", Attrs, fun unicode:characters_to_binary/1),
        name = attr("name", Attrs, fun unicode:characters_to_binary/1)
      },
      {N + 1, [{N, fix_record(Rec)} | Acc]};
    (_, _, Acc) ->
      Acc
  end,
  {ok, {_N, Langs}, _} = xmerl_sax_parser:file("/usr/share/xml/iso-codes/iso_639.xml",
    [{event_state, {1, []}},
      {event_fun, EventFun}]
  ),
  [
    format_body(<<"languages_data">>, Langs),
    format_index(
      [{<<"alpha2">>, <<"Alpha">>, 2}, {<<"bibliographic">>, <<"Bib">>, 3}, {<<"terminology">>, <<"Termin">>, 3}],
      <<"languages_index">>, Langs, []
    )
  ].


build_scripts() ->
  EventFun = fun
    ({startElement, _, "iso_15924_entry", _QName, Attrs}, _, {N, Acc}) ->
      Rec = #script{
        alpha4 = attr("alpha_4_code", Attrs, fun unicode:characters_to_binary/1),
        numeric = attr("numeric_code", Attrs, fun unicode:characters_to_binary/1),
        name = attr("name", Attrs, fun unicode:characters_to_binary/1)
      },
      {N + 1, [{N, fix_record(Rec)} | Acc]};
    (_, _, Acc) ->
      Acc
  end,
  {ok, {_N, Scripts}, _} = xmerl_sax_parser:file("/usr/share/xml/iso-codes/iso_15924.xml",
    [{event_state, {1, []}},
      {event_fun, EventFun}]
  ),
  [
    format_body(<<"scripts_data">>, Scripts),
    format_index([{<<"alpha4">>, <<"Alpha">>, 2}, {<<"numeric">>, <<"Numeric">>, 3}], <<"scripts_index">>, Scripts, [])
  ].

build_countries() ->
  EventFun = fun
    ({startElement, _, "iso_3166_entry", _QName, Attrs}, _, {{N, AccLive}, Dead}) ->
      Rec = #country{
        alpha2 = attr("alpha_2_code", Attrs, fun unicode:characters_to_binary/1),
        alpha3 = attr("alpha_3_code", Attrs, fun unicode:characters_to_binary/1),
        numeric = attr("numeric_code", Attrs, fun unicode:characters_to_binary/1),
        name = attr("name", Attrs, fun unicode:characters_to_binary/1),
        official_name = attr("official_name", Attrs, fun unicode:characters_to_binary/1)
      },
      {{N + 1, [{N, fix_record(Rec)} | AccLive]}, Dead};
    ({startElement, _, "iso_3166_3_entry", _QName, Attrs}, _, {Live, {M, AccDead}}) ->
      Rec = #historic_country{
        alpha2 = attr("alpha_4_code", Attrs, fun unicode:characters_to_binary/1),
        alpha3 = attr("alpha_3_code", Attrs, fun unicode:characters_to_binary/1),
        numeric = attr("numeric_code", Attrs, fun unicode:characters_to_binary/1),
        names = attr("names", Attrs, fun unicode:characters_to_binary/1),
        date_withdrawn = attr("date_withdrawn", Attrs, fun parse_date/1)
      },

      {Live, {M + 1, [{M, fix_record(Rec)} | AccDead]}};

    (_, _, Acc) ->
      Acc
  end,
  {ok, {{_N, LiveCountries}, {_M, DeadCountries}}, _} = xmerl_sax_parser:file("/usr/share/xml/iso-codes/iso_3166.xml",
    [{event_state, {{1, []}, {1, []}}},
      {event_fun, EventFun}]
  ),

  [
    format_body(<<"countries_data">>, LiveCountries),
    format_body(<<"countries_3_data">>, DeadCountries),
    format_index(
      [{<<"alpha3">>, <<"Alpha3">>, 2}, {<<"alpha2">>, <<"Alpha2">>, 3}, {<<"numeric">>, <<"Numeric">>, 4}],
      <<"countries_index">>, LiveCountries, []
    ),
    format_index(
      [{<<"alpha3">>, <<"Alpha3">>, 2}, {<<"alpha2">>, <<"Alpha2">>, 3}, {<<"numeric">>, <<"Numeric">>, 4}],
      <<"countries_3_index">>, DeadCountries, []
    )
  ].


build_currencies() ->
  {ok, [Symbols]} = file:consult("priv/data/symbols.term"),
  Symbol = fun(Letter) ->
    proplists:get_value(Letter, Symbols, <<>>)
  end,
  EventFun = fun
    ({startElement, _, "iso_4217_entry", _QName, Attrs}, _, {{N, AccLive}, Dead}) ->
      Letter = attr("letter_code", Attrs, fun unicode:characters_to_binary/1),
      Rec = #currency{
        letter = Letter,
        numeric = attr("numeric_code", Attrs, fun unicode:characters_to_binary/1),
        symbol = Symbol(Letter),
        name = attr("currency_name", Attrs, fun unicode:characters_to_binary/1)
      },
      {{N + 1, [{N, fix_record(Rec)} | AccLive]}, Dead};
    ({startElement, _, "historic_iso_4217_entry", _QName, Attrs}, _, {Live, {M, AccDead}}) ->
      Letter = attr("letter_code", Attrs, fun unicode:characters_to_binary/1),
      Rec = #historic_currency{
        letter = Letter,
        numeric = attr("numeric_code", Attrs, fun unicode:characters_to_binary/1),
        symbol = Symbol(Letter),
        name = attr("currency_name", Attrs, fun unicode:characters_to_binary/1),
        date_withdrawn = attr("date_withdrawn", Attrs, fun parse_date/1)
      },

      {Live, {M + 1, [{M, fix_record(Rec)} | AccDead]}};

    (_, _, Acc) ->
      Acc
  end,
  {ok, {{_N, LiveCurrencies}, {_M, DeadCurrencies}}, _} = xmerl_sax_parser:file("/usr/share/xml/iso-codes/iso_4217.xml",
    [{event_state, {{1, []}, {1, []}}},
      {event_fun, EventFun}]
  ),


  [
    format_body(<<"currencies_data">>, LiveCurrencies),
    format_body(<<"historic_currencies_data">>, DeadCurrencies),
    format_index(
      [{<<"letter">>, <<"Letter">>, 2}, {<<"numeric">>, <<"Numeric">>, 3}],
      <<"currencies_index">>, LiveCurrencies, []
    ),
    format_index(
      [{<<"letter">>, <<"Letter">>, 2}, {<<"numeric">>, <<"Numeric">>, 3}],
      <<"historic_currencies_index">>, DeadCurrencies, []
    )
  ].





build_header() ->
  {{YY, MM, DD}, {Hour, Min, Sec}} = _Now = calendar:local_time(),
  [<<"-module(world_data).\n\n">>,
    io_lib:format(
      <<"%% Autogenerated by world_builder at ~4..0w-~2..0w-~2..0w ~2..0w:~2..0w.~p \n\n">>,
      [YY, MM, DD, Hour, Min, Sec]
    ),
    <<"%% API\n-export([scripts_data/0, scripts_index/1, countries_data/0, countries_3_data/0, countries_index/1,
      countries_3_index/1, languages_data/0, languages_index/1, currencies_data/0, historic_currencies_data/0,
      currencies_index/1, historic_currencies_index/1]).">>
  ].
build_body() ->
  [build_scripts(), build_countries(), build_languages(), build_currencies()].





build_iolist() ->
  {ok, [build_header(), build_body()]}.


build() ->

  {ok, Data} = build_iolist(),
  {ok, TZOut} = file:open("src/world_data.erl~", [write, raw, binary]),
  file:write(TZOut, Data),
  case filelib:is_file("src/world_data.erl") of
    true ->
      file:delete("src/world_data.erl");
    _ -> ok
  end,
  file:rename("src/world_data.erl~", "src/world_data.erl"),
  ok.