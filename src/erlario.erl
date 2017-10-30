-module(erlario).

%% API exports
-export([name_of_month/2,name_of_day/4,month_all_days/2,year_all_month/2,
        month_to_weeks/2,next_day/1,year_all_days/1,date_to_binary/1,
        year_to_ets/2]).

-define(WEEK_NAME,{'Lunes','Martes','Miércoles','Jueves','Viernes','Sábado','Domingo'}).
-define(MONTH_NAME,{'Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto',
                    'Septiembre','Octubre','Noviembre','Diciembre'}).

-record(date,{year::calendar:year(),month::calendar:month(),day::calendar:day()}).
-record(id_date, {id::binary(), fecha=#date{}}).

-export_type([id_date/0, date/0]).
-type id_date() :: #id_date{}.
-type date() :: #date{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Nombre del mes
%
-spec name_of_month(Day, Lang) -> string() when
    Day  :: calendar:day(),
    Lang :: {string()}.
name_of_month(Day,Lang) ->
    element(Day,Lang).

%% @doc Nombre del día de la semana.
%
-spec name_of_day(Day, Month, Year, Lang) -> string() when
    Day  :: calendar:day(),
    Month:: calendar:month(),
    Year :: calendar:year(),
    Lang :: {string()}.
name_of_day(Day,Month,Year,Lang) ->
    element(calendar:day_of_the_week(Year,Month,Day),Lang).

%% @doc Lista con todos los días del mes.
%
-spec month_all_days(Month,Year) -> [calendar:day()] when
    Month  :: calendar:month(),
    Year   :: calendar:year().
month_all_days(Month, Year) ->
    lists:seq(1,calendar:last_day_of_the_month(Year,Month)).

%% @doc Mapa con todos los meses del año, el key es el nombre del mes y el value es
%% una lista con todos los días de ese mes.
%
-spec year_all_month(Year,Lang) -> map() when
    Year :: calendar:year(),
    Lang:: {list()}.
year_all_month(Year, Lang) ->
    Result = maps:new(),
    year_all_month(Result, 12, Year, Lang).

-spec year_all_month(Result, Count, Year, Lang) -> Result when
    Result :: map(),
    Count  :: calendar:month(),
    Year   :: calendar:year(),
    Lang   :: {list()}.
year_all_month(Result, 0, _Year, _Lang) ->
    Result;
year_all_month(Result, Count, Year, Lang) ->
    Nombre = name_of_month(Count,Lang),
    Dias = month_all_days(Count,Year),
    NuevoResult = maps:put(Nombre,Dias,Result),
    year_all_month(NuevoResult, Count -1, Year, Lang).

%% @doc Se obtiene una lista de listas con todos los días del año
%% separados por meses.
% 
-spec year_all_days(Year) -> [list()] when
    Year :: calendar:year().
year_all_days(Year) ->  year_all_days(Year, 1, []).
year_all_days(_Year, 13, Acc) -> lists:reverse(Acc);
year_all_days(Year, Month, Acc) ->
    Mes = month_all_days(Month, Year),
    year_all_days(Year, Month + 1, [Mes|Acc]).

%% @doc Devuelve una lista de listas con todos los días del mes separados por
%% semanas
%
-spec month_to_weeks(Month,Year) -> [[calendar:day()]] when
    Month :: calendar:month() | list(),
    Year  :: calendar:year() | list().
month_to_weeks(Month, Year) when is_integer(Month), is_integer(Year)->
    Mes = month_all_days(Month, Year),
    DayOfWeek = calendar:day_of_the_week(Year,Month,1),
    Zero = lists:duplicate(DayOfWeek - 1, 0),
    {H,T} = lists:split(8-DayOfWeek,Mes),
    First = lists:append(Zero,H),
    month_to_weeks(T, [First]);
month_to_weeks([], Acc) ->
    lists:reverse(Acc);
month_to_weeks(List, Acc) when length(List) < 7 ->
    Zero = lists:duplicate(7 - length(List), 0),
    H = lists:append(List,Zero),
    month_to_weeks([],[H | Acc]);
month_to_weeks(List, Acc) when is_list(Acc), is_list(List) ->
    {H,T} = lists:split(7,List),
    month_to_weeks(T, [H | Acc]).

%% @doc Devuelve el siguiente día en formato
%% {Year,Month,Day} 
%
-spec next_day(Fecha) -> calendar:date() when
        Fecha :: calendar:date().
next_day({Year,Month,Day}) ->
    calendar:gregorian_days_to_date(
            calendar:date_to_gregorian_days({Year,Month,Day}) + 1).

%% @doc Convierte la primera 3-tupla de tipo #date{} en binary
%% para poder usarlo de clave primaria en ets y así poder
%% ordenar por fecha
%
-spec date_to_binary(Tuple) -> binary() when
    Tuple :: #date{}.
date_to_binary(#date{year=Year,month=Month,day=Day}) ->
    Corrector = fun(Num) when Num < 10 ->
                        "0" ++ integer_to_list(Num);
                   (Num) when Num >= 10 ->
                        integer_to_list(Num) end,
    Corregido = [Corrector(X) || X <- [Year, Month, Day]],
    list_to_binary(Corregido).

%% @doc De un año y las opciones para ets se obtiene en una
%% tabla ets los dias del año en formato #id_date
%
-spec year_to_ets(Year, Opts) -> ets:tid() when
    Year :: calendar:year(),
    Opts :: list().
year_to_ets(Year, Opts) ->
    Year_to_dates = fun(Lista) ->
        {R,_} = lists:mapfoldl(fun(Item, Idx)->
                {lists:map(fun(D)-> #date{year=Year,month=Idx,day=D} end, Item), Idx +1}
                end,1,Lista),
        R
    end,
    Dates = lists:append(Year_to_dates(year_all_days(Year))),
    Todos = [#id_date{id=date_to_binary(X),fecha=X} || X <- Dates],
    Dbase = ets:new(calen, [{keypos, #id_date.id} | Opts]),
    ets:insert(Dbase, Todos),
    Dbase.

%%============================================================
%% Tests
%% ===========================================================

-ifdef(TEST).

month_all_days_test() ->
    ?assertEqual([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],
                 month_all_days(9,2017)).

name_of_day_test() ->
    ?assertEqual('Miércoles', name_of_day(21,9,2016,?WEEK_NAME)),
    ?assertEqual('Viernes', name_of_day(1,9,2017,?WEEK_NAME)).

name_of_month_test() ->
    ?assertEqual('Octubre', name_of_month(10,?MONTH_NAME)),
    ?assertEqual('Enero', name_of_month(1,?MONTH_NAME)).

month_to_weeks_test() ->
    ?assertEqual([[0,0,0,0,1,2,3],
                 [4,5,6,7,8,9,10],
                 [11,12,13,14,15,16,17],
                 [18,19,20,21,22,23,24],
                 [25,26,27,28,29,30,0]], month_to_weeks(9,2017)).

next_day_test() ->
    ?assertEqual({2017,10,3},next_day({2017,10,2})),
    ?assertEqual({2017,11,1},next_day({2017,10,31})),
    ?assertEqual({2018,1,1},next_day({2017,12,31})).

-endif.
