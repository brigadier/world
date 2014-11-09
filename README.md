world
=========

ISO countries, languages, currencies and scripts definitions in Erlang.
The app provides interface to the following standards:

639
    Languages
3166
    Countries and historic countries
4217
    Currencies and historic currencies
15924
    Scripts

Don't expect it to 100% conform to ISO standards, it has deliberate tweaks incompativle with ISO.

The library contains world_builder.erl module to build data module (world_data.erl) from the XML files supplied
with Linux (/usr/share/xml/iso-codes/). Also it contains prebuilt world_data.erl with actual
data from Gentoo Linux, November 2014. You can't build world_data.erl on the platforms
with no /usr/share/xml/iso-codes/, such as Windows, but you can use it on any platform with the prebuilt world_data.erl.

Overall, it's very similar to pycountries.


Build with prebuit world_data.erl
-----

    make


Build, create new world_data.erl and compile it
-----

    make compile data compile


Usage
-----

Include file include/world.hrl contains the following records to use instead of tuples:  script, country,
historic_country, language,  currency, historic_currency. Everywhere in the examples, when you see a tagged tuple,
assume you can use record with the same name. Some fields may contain 'undefined', some not, look up
typespecs in world.hrl.
All lookups are case sensitive.
Numeric codes are stored as binaries with leading zeros.
Currencies and countries can be live or historic, you can lookup either or both using Options - ['live'] for live ones,
['historic'] for historic ones, ['live', 'historic'] for both. By default, when the list is empty or
absent, it's 'live' only.

$ make compile run

Countries:

    1> {ok, CountriesList} = world:countries().
        {ok,[{country,<<"AFG">>,<<"AF">>,<<"004">>,
                  <<"Afghanistan">>,<<"Islamic Republic of Afghanistan">>},
         {country,<<"ALA">>,<<"AX">>,<<"248">>,
                  <<195,133,108,97,110,100,32,73,115,108,97,110,100,115>>,
                  <<195,133,108,97,110,100,32,73,115,108,97,110,100,115>>},
         {country,<<"ALB">>,<<"AL">>,<<"008">>,<<"Albania">>,
                  <<"Republic of Albania">>},
            ....

    2> {ok, CountriesList, HistoricCountriesList} = world:countries([live, historic]).
        {ok,[{country,<<"AFG">>,<<"AF">>,<<"004">>,
                  <<"Afghanistan">>,<<"Islamic Republic of Afghanistan">>},
         {country,<<"ALA">>,<<"AX">>,<<"248">>,
                  <<195,133,108,97,110,100,32,73,115,108,97,110,100,115>>,
                  <<195,133,108,97,110,100,32,73,115,108,97,110,100,115>>},
            ....,
         [{historic_country,<<"ATB">>,<<"BQ">>,undefined,
                           <<"British Antarctic Territory">>,
                           {1979}},
         {historic_country,<<"BUR">>,<<"BU">>,<<"104">>,
                           <<"Burma, Socialist Republic of the Union of">>,
                           {1989,12,5}},
            ....

    3> world:country(<<"FR">>).
        {ok,{country,<<"FRA">>,<<"FR">>,<<"250">>,<<"France">>,
                 <<"French Republic">>}}

    4> world:country(<<"DD">>).
        undefined

    5> world:country(<<"DD">>, [live, historic]).
        {ok,{historic_country,<<"DDR">>,<<"DD">>,<<"278">>,
                          <<"German Democratic Republic">>,
                          {1990,10,30}}}

    6> world:country({alpha2, <<"DD">>}, [live, historic]).
        {ok,{historic_country,<<"DDR">>,<<"DD">>,<<"278">>,
                          <<"German Democratic Republic">>,
                          {1990,10,30}}}

    7> world:country({alpha3, <<"DDR">>}, [live, historic]).
        {ok,{historic_country,<<"DDR">>,<<"DD">>,<<"278">>,
                          <<"German Democratic Republic">>,
                          {1990,10,30}}}

    8> world:country({numeric, <<"278">>}, [historic]).
        {ok,{historic_country,<<"DDR">>,<<"DD">>,<<"278">>,
                          <<"German Democratic Republic">>,
                          {1990,10,30}}}

Languages:

    9> world:languages().
        {ok,[{language,<<"aa">>,<<"aar">>,<<"aar">>,<<"Afar">>},
             {language,<<"ab">>,<<"abk">>,<<"abk">>,<<"Abkhazian">>},
             {language,undefined,<<"ace">>,<<"ace">>,<<"Achinese">>},
             {language,undefined,<<"ach">>,<<"ach">>,<<"Acoli">>},
                ....

    10> world:language(<<"am">>).
        {ok,{language,<<"am">>,<<"amh">>,<<"amh">>,<<"Amharic">>}}

    11> world:language({alpha2, <<"am">>}).
        {ok,{language,<<"am">>,<<"amh">>,<<"amh">>,<<"Amharic">>}}

    12> world:language({terminology, <<"amh">>}).
        {ok,{language,<<"am">>,<<"amh">>,<<"amh">>,<<"Amharic">>}}

    13> world:language({bibliographic, <<"amh">>}).
        {ok,{language,<<"am">>,<<"amh">>,<<"amh">>,<<"Amharic">>}}


Scripts:

    14> world:scripts().
        {ok,[{script,<<"Afak">>,<<"439">>,<<"Afaka">>},
             {script,<<"Aghb">>,<<"239">>,<<"Caucasian Albanian">>},
             {script,<<"Ahom">>,<<"338">>,<<"Ahom">>},
             {script,<<"Arab">>,<<"160">>,<<"Arabic">>},
             {script,<<"Armi">>,<<"124">>,<<"Imperial Aramaic">>},
                ....

    15> world:script({numeric, <<"220">>}).
        {ok,{script,<<"Cyrl">>,<<"220">>,<<"Cyrillic">>}}

    16> world:script({alpha4, <<"Latn">>}).
        {ok,{script,<<"Latn">>,<<"215">>,<<"Latin">>}}


Currencies:

    17> world:currencies().
        {ok,[{currency,<<"AED">>,<<"784">>,<<>>,<<"UAE Dirham">>},
             {currency,<<"AFN">>,<<"971">>,<<216,139>>,<<"Afghani">>},
             {currency,<<"ALL">>,<<"008">>,<<"Lek">>,<<"Lek">>},
             {currency,<<"AMD">>,<<"051">>,<<>>,<<"Armenian Dram">>},
             {currency,<<"ANG">>,<<"532">>,
                ....

    18> world:currencies([live, historic]).
        {ok,[{currency,<<"AED">>,<<"784">>,<<>>,<<"UAE Dirham">>},
             {currency,<<"AFN">>,<<"971">>,<<216,139>>,<<"Afghani">>},
             {currency,<<"ALL">>,<<"008">>,<<"Lek">>,<<"Lek">>},
                    ....,
            [{historic_currency,<<"ADP">>,<<"020">>,<<>>,
                                <<"Andorran Peseta">>,
                                {2002,3}},
             {historic_currency,<<"ADF">>,undefined,<<>>,
                                <<"Andorran Franc">>,undefined},
             {historic_currency,<<"AFA">>,<<"004">>,<<>>,<<"Afghani">>,
                                undefined},
                ....

    19> world:currency(<<"USD">>).
        {ok,{currency,<<"USD">>,<<"840">>,<<"$">>,<<"US Dollar">>}}

    20> world:currency(<<"USD">>, [historic]).
        undefined

    21> world:currency({letter, <<"USD">>}, [live]).
        {ok,{currency,<<"USD">>,<<"840">>,<<"$">>,<<"US Dollar">>}}

    22> world:currency({numeric, <<"840">>}, [live, historic]).
        {ok,{currency,<<"USD">>,<<"840">>,<<"$">>,<<"US Dollar">>}}


