%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Mustach template engine for Erlang/OTP.
-module(mustache).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         new/1,
         parse_file/1,
         compile/2
        ]).

-export_type([
              template/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Defines & Records & Types
%%----------------------------------------------------------------------------------------------------------------------

-define(PARSE_ERROR, incorrect_format).
-define(COND(Cond, TValue, FValue),
        case Cond of true -> TValue; false -> FValue end).

-type key()  :: binary().
-type tag()  :: {n,   key()}          |
                {'&', key()}          |
                {'#', key(), [tag()]} |
                {'^', key(), [tag()]} |
                {'>', key()}          |
                binary().
-type partial() :: {partial, {EndTag :: binary(), Rest :: binary(), [tag()]}}.

-record(state,
        {
          dirname = <<>>     :: filename:filename_all(),
          start   = <<"{{">> :: binary(),
          stop    = <<"}}">> :: binary()
        }).
-type state() :: #state{}.

-record(?MODULE,
        {
          data :: [tag()]
        }).

-type template() :: #?MODULE{}.
-type data()     :: #{string() => data() | iodata() | fun((data(), function()) -> iodata())}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @doc
-spec new(binary()) -> template().
new(Bin) when is_binary(Bin) ->
    new_impl(#state{}, Bin).

%% @doc
-spec parse_file(file:filename()) -> template().
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} -> new_impl(#state{dirname = filename:dirname(Filename)}, Bin);
        _         -> error(file_not_found, [Filename])
    end.

%% @doc
-spec compile(template(), data()) -> binary().
compile(#?MODULE{}, Map) when is_map(Map) ->
    hoge.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------------------------------

%% @see new/1
-spec new_impl(state(), Input :: binary()) -> template().
new_impl(State, Input) ->
    #?MODULE{data = parse(State, Input)}.

%% @doc
-spec parse(state(), binary()) -> [tag()].
parse(State, Bin) ->
    case parse1(State, Bin, []) of
        {partial, _} -> error(?PARSE_ERROR);
        Tags         -> lists:reverse(Tags)
    end.

%% @doc 1st phase of the `parse/1'
%%
%% ATTENTION: The result is a list that is inverted.
-spec parse1(state(), Input :: binary(), Result :: [tag()]) -> [tag()] | partial().
parse1(#state{start = Start} = State, Bin, Result) ->
    %% `<<"hoge{{tag}}foo">>' => `[<<"hoge">>, <<"tag}}foo">>]'
    case binary:split(Bin, Start) of
        []       -> Result;
        [B1]     -> [B1 | Result];
        [B1, B2] -> parse2(State, B2, [B1 | Result])
    end.

%% @doc 2nd phase of the `parse/1'
%%
%% ATTENTION: The result is list that is inverted.
-spec parse2(state(), Input :: binary(), Result :: [tag()]) -> [tag()] | partial().
parse2(#state{stop = Stop} = State, Bin, Result) ->
    %% `<<"hoge}}foo...">>' => `[<<"hoge">>, <<"foo...">>]'
    case binary:split(Bin, Stop) of
        [B1, B2] ->
            %% TODO: must not replace !!
            Tag0 = binary:replace(B1, <<" ">>, <<>>, [global]),
            case Tag0 of
                <<"{", Tag/binary>> ->
                    case B2 of
                        <<"}", B3/binary>> -> parse1(State, B3, [{'&', Tag} | Result]);
                        _                  -> error(?PARSE_ERROR)
                    end;
                <<"&", Tag/binary>> ->
                    parse1(State, B2, [{'&', Tag} | Result]);
                <<T, Tag/binary>> when T =:= $#; T =:= $^ ->
                    parse_loop(State, ?COND(T =:= $#, '#', '^'), Tag, B2, Result);
                <<"=", _/binary>> ->
                    parse_delimiter(State, B1, B2, Result);
                <<"!", _/binary>> ->
                    parse1(State, B2, Result);
                <<"/", Tag/binary>> ->
                    {partial, {State, Tag, B2, Result}};
                _  ->
                    parse1(State, B2, [{n, Tag0} | Result])
            end;
        _  -> error(?PARSE_ERROR)
    end.

%% @doc Loop processing part of the `parse/1'
%%
%% `{{# Tag}}' or `{{^ Tag}}' corresponds to this.
-spec parse_loop(state(), '#' | '^', Tag :: binary(), Input :: binary(), Result :: [tag()]) -> [tag()] | partial().
parse_loop(State0, Mark, Tag, Input, Result0) ->
    case parse1(State0, Input, []) of
        {partial, {State, Tag, Rest, Result1}} when is_list(Result1) ->
            parse1(State, Rest, [{Mark, Tag, lists:reverse(Result1)} | Result0]);
        _ ->
            error(?PARSE_ERROR)
    end.

%% @doc Update delimiter part of the `parse/1'
%%
%% `{{=NewStartDelimiter NewStopDelimiter=}}' coresspond to this. <br />
%% NewDelimiterBin is refers to a binary up to `=' from `='. <br />
%% (e.g. `{{=%% %%=}}' -> `=%% %%=')
-spec parse_delimiter(state(), NewDelimiterBin :: binary(), NextBin :: binary(), Result :: [tag()]) -> [tag()] | partial().
parse_delimiter(State0, NewDelimiterBin, NextBin, Result) ->
    State =
        try
            [_BeforeTheEqual, Delimiters, _AfterTheEqual] = binary:split(NewDelimiterBin, <<"=">>, [global]),
            [StartDelimiter | T] = binary:split(Delimiters, <<" ">>, [global]),
            [StopDelimiter  | _] = lists:reverse(T),
            State0#state{start = StartDelimiter, stop = StopDelimiter}
        catch
            error:{badmatch, _} -> error(?PARSE_ERROR)
        end,
    parse1(State, NextBin, Result).
