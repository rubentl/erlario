-module(elpdf).

-export([to_file/2]).
-compile([export_all]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

fuente() -> "Helvetica".
fuente_bold() -> "Helvetica-Bold".
tamano() -> 12.
tamano_large() -> 40.

%% @doc Crea un archivo pdf con el calendario del año
%%
-spec to_file(Anno::calendar:year(), File::file:filename()) -> ok.
to_file(Anno, File) ->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,a4),
    
    anno(PDF, Anno),
    
    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(File,[Serialised]),
    eg_pdf:delete(PDF),
    ok.

%% @doc Compone un año completo en el stream PDF obtenido con 
%% eg_pdf:new()
%%
-spec anno(PDF::pid(), Anno::calendar:year()) -> ok.
anno(PDF, Anno) ->
    titulo(PDF,Anno),
    lists:foreach(fun(Mes)-> 
                      mes_tabla(PDF, Anno, Mes) end,
                  lists:seq(1,12)),
    ok.

%% @doc Compone el mes de un Año en el stream 
%%
-spec mes_tabla(PDF, Anno, Mes) -> ok when
      PDF :: pid(),
      Anno :: calendar:year(),
      Mes :: calendar:month().
mes_tabla(PDF, Anno, Mes) ->
    mes_cabecera(PDF, Mes),
    Semanas = erlario:month_to_weeks(Mes, Anno),
    lists:foldl(
        fun(Item, Acc) -> 
            mes_semana(PDF, Mes, Acc, Item),
            Acc + 1
        end,
        1,
        Semanas),
    ok.

%% @doc Compone la cabecera del mes con el nombre del mes y las siglas de los dias
%% de la semana.
%%
-spec mes_cabecera(PDF::pid(), Mes::calendar:month()) -> ok.
mes_cabecera(PDF, Mes) ->
    NombreMes = lists:nth(Mes,mes_nombre()),
    {X, Y, Ancho, _Alto} = mes_xy(Mes),
    texto_centrar(PDF, fuente_bold(), tamano() + 2, X + (Ancho/2), Y, NombreMes),
    DiaX = Ancho / 7,
    fila(PDF, X + (DiaX / 2), Y - 20, DiaX, dia_nombre()),
    ok.

%% @doc Compone la semana del mes 
%%
-spec mes_semana(PDF, Mes, Semana, Dias) -> ok when
      PDF :: pid(),
      Mes :: calendar:month(),
      Semana :: 1..6,
      Dias :: [calendar:day()].
mes_semana(PDF, Mes, Semana, Dias) ->
    {X, Y, Ancho, _Alto} = mes_xy(Mes),
    AnchoDia = Ancho /7,
    Fila = Y - (Semana * 20),
    fila(PDF, X + (AnchoDia / 2), Fila - 20, AnchoDia, Dias),
    ok.

%% @doc Compone una fila en un mes con el contenido pasado en la lista.
%%
-spec fila(PDF, X, Y, IncX, Lista) -> ok when
      PDF :: pid(),
      X :: integer() | float(),
      Y :: integer() | float(),
      IncX :: integer() | float(),
      Lista :: list().
fila(_PDF, _X, _Y, _IncX, []) -> ok;
fila(PDF, X, Y, IncX, [H|T]) ->
    texto_centrar(PDF, X, Y, H),
    fila(PDF, X + IncX, Y, IncX, T).

%% @doc Devuelve las coordenadas de un mes en una página a4.
%%
-spec mes_xy(Num::calendar:month()) -> {X, Y, Ancho, Alto} when
      X :: integer() | float(),
      Y :: integer() | float(),
      Ancho :: integer() | float(),
      Alto :: integer() | float().
mes_xy(Num) ->
    {_, _, AnchoPagina, AltoPagina} = eg_pdf:pagesize(a4),
    Alto = trunc((AltoPagina - 100) / 4),
    Fil = [{lists:member(Num, [1,2,3]),4},
           {lists:member(Num, [4,5,6]),3},
           {lists:member(Num, [7,8,9]),2},
           {lists:member(Num, [10,11,12]),1}],
    {true, Fila} = lists:keyfind(true,1,Fil),
    Ancho = trunc((AnchoPagina - 100) / 3),
    Col = [{lists:member(Num, [1,4,7,10]),0},
           {lists:member(Num, [2,5,8,11]),1},
           {lists:member(Num, [3,6,9,12]),2}],
    {true, Columna} = lists:keyfind(true,1,Col),
    {25 + (25*Columna) + (Ancho*Columna) , Alto*Fila, Ancho, Alto}.


%% @doc Compone en el stream PDF el título de la página
%%
-spec titulo(PDF::pid(), Str::string()) -> ok.
titulo(PDF, Str) ->
    AnchoTexto = eg_pdf:get_string_width(PDF, fuente_bold(), tamano_large(), int_to_txt(Str)),
    {_, _, AnchoPagina, AltoPagina} = eg_pdf:pagesize(a4),
    X = (AnchoPagina - AnchoTexto) / 2,
    Y = AltoPagina - 50,
    texto(PDF, fuente_bold(), tamano_large(), X, Y, Str),
    ok.


%% @doc Devuelve un list() ya sea un número o un list() el parámetro.
%%
-spec int_to_txt(T::number() | list()) -> list().
int_to_txt(0) -> "";
int_to_txt(T) when is_integer(T) -> integer_to_list(T);
int_to_txt(T) when is_list(T) -> T.


%% @doc Compone un texto en el stream PDF
%%
-spec texto(PDF, Fuente, Tamano, X, Y, Str) -> ok when
      PDF :: pid(),
      Fuente :: string(),
      Tamano :: number(),
      X :: integer() | float(),
      Y :: integer() | float(),
      Str :: string().
texto(PDF, Fuente, Tamano, X, Y, Str) ->
    eg_pdf:set_font(PDF, Fuente, Tamano),
    eg_pdf:moveAndShow(PDF, X, Y, int_to_txt(Str)),
    ok.

%% @doc Compone un texto en el stream PDF con los valores de Tamaño y Fuente 
%% predeterminadas
%%
-spec texto(PDF, X, Y, Str) -> ok when
      PDF :: pid(),
      X :: integer() | float(),
      Y :: integer() | float(),
      Str :: string().
texto(PDF, X, Y, Str) ->
    texto(PDF, fuente(), tamano(), X, Y, Str),
    ok.

%% @doc Compone un texto centrado en el stream PDF
%%
-spec texto_centrar(PDF, Fuente, Tamano, X, Y, Str) -> ok when
      PDF :: pid(),
      Fuente :: string(),
      Tamano :: number(),
      X :: integer() | float(),
      Y :: integer() | float(),
      Str :: string().
texto_centrar(PDF, Fuente, Tamano, X, Y, Str) ->
    AnchoTexto = eg_pdf:get_string_width(PDF, Fuente, Tamano, int_to_txt(Str)),
    texto(PDF, Fuente, Tamano, X - (AnchoTexto/2), Y, Str),
    ok.

%% @doc Compone un texto centrado en el stream PDF con los valores de Tamaño y Fuente 
%% predeterminadas
%%
-spec texto_centrar(PDF, X, Y, Str) -> ok when
      PDF :: pid(),
      X :: integer() | float(),
      Y :: integer() | float(),
      Str :: string().
texto_centrar(PDF, X, Y, Str) ->
    texto_centrar(PDF, fuente(), tamano(), X, Y, Str),
    ok.
    
dia_nombre() -> ["L","M","X","J","V","S","D"].
mes_nombre() -> ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
    "Septiembre","Octubre","Noviembre","Diciembre"].



-ifdef(TEST).

prueba_test() ->
    ?assertEqual(ok, to_file(2017, "priv/elpdf.pdf")).

-endif.
