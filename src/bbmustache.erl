%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Binary pattern match Based Mustach template engine for Erlang/OTP.
-module(bbmustache).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         render/2,
         render/3,
         parse_binary/1,
         parse_file/1,
         compile/2,
         compile/3
        ]).

-export_type([
              template/0,
              data/0,
              option/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Defines & Records & Types
%%----------------------------------------------------------------------------------------------------------------------

-define(PARSE_ERROR, incorrect_format).
-define(FILE_ERROR,  file_not_found).
-define(COND(Cond, TValue, FValue),
        case Cond of true -> TValue; false -> FValue end).

-type key()  :: binary().
-type tag()  :: {n,   key()}                              |
                {'&', key()}                              |
                {'#', key(), [tag()], Source :: binary()} |
                {'^', key(), [tag()]}                     |
                binary().

-record(?MODULE,
        {
          data :: [tag()]
        }).

-opaque template() :: #?MODULE{}.
%% @see parse_binary/1
%% @see parse_file/1

-record(state,
        {
          dirname = <<>>     :: file:filename_all(),
          start   = <<"{{">> :: binary(),
          stop    = <<"}}">> :: binary()
        }).
-type state() :: #state{}.

-type data_key()   :: atom() | binary() | string().
%% You can choose one from these as the type of key in {@link data/0}.

-type data_value() :: data() | iodata() | number() | atom() | fun((data(), function()) -> iodata()).
%% Function is intended to support a lambda expression.

-type assoc_data() :: [{atom(), data_value()}] | [{binary(), data_value()}] | [{string(), data_value()}].

-type option()     :: {key_type, atom | binary | string}.
%% - key_type: Specify the type of the key in {@link data/0}. Default value is `string'.

-ifdef(namespaced_types).
-type maps_data() :: #{atom() => data_value()} | #{binary() => data_value()} | #{string() => data_value()}.
-type data()      :: maps_data() | assoc_data().
-else.
-type data()      :: assoc_data().
-endif.
%% All key in assoc list or maps must be same type.
%% @see render/2
%% @see compile/2

-type endtag()    :: {endtag, {state(), EndTag :: binary(), LastTagSize :: non_neg_integer(), Rest :: binary(), Result :: [tag()]}}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @equiv render(Bin, Data, [])
-spec render(binary(), data()) -> binary().
render(Bin, Data) ->
    render(Bin, Data, []).

%% @equiv compile(parse_binary(Bin), Data, Options)
-spec render(binary(), data(), [option()]) -> binary().
render(Bin, Data, Options) ->
    compile(parse_binary(Bin), Data, Options).

%% @doc Create a {@link template/0} from a binary.
-spec parse_binary(binary()) -> template().
parse_binary(Bin) when is_binary(Bin) ->
    parse_binary_impl(#state{}, Bin).

%% @doc Create a {@link template/0} from a file.
-spec parse_file(file:filename_all()) -> template().
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} -> parse_binary_impl(#state{dirname = filename:dirname(Filename)}, Bin);
        _         -> error(?FILE_ERROR, [Filename])
    end.

%% @equiv compile(Template, Data, [])
-spec compile(template(), data()) -> binary().
compile(Template, Data) ->
    compile(Template, Data, []).

%% @doc Embed the data in the template.
%%
%% ```
%% 1> Template = bbmustache:parse_binary(<<"{{name}}">>).
%% 2> bbmustache:compile(Template, #{"name" => "Alice"}).
%% <<"Alice">>
%% '''
%% Data support assoc list or maps (OTP17 or later). <br />
%% All key in assoc list or maps must be same type.
-spec compile(template(), data(), [option()]) -> binary().
compile(#?MODULE{data = Tags} = T, Data, Options) ->
    case check_data_type(Data) of
        false -> error(function_clause, [T, Data]);
        _     -> iolist_to_binary(lists:reverse(compile_impl(Tags, Data, [], Options)))
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------------------------------

%% @doc {@link compile/2}
%%
%% ATTENTION: The result is a list that is inverted.
-spec compile_impl(Template :: [tag()], data(), Result :: iodata(), Options :: [option()]) -> iodata().
compile_impl([], _, Result, _) ->
    Result;
compile_impl([{n, Key} | T], Map, Result, Options) ->
    compile_impl(T, Map, [escape(to_iodata(data_get(convert_keytype(Key, Options), Map, <<>>))) | Result], Options);
compile_impl([{'&', Key} | T], Map, Result, Options) ->
    compile_impl(T, Map, [to_iodata(data_get(convert_keytype(Key, Options), Map, <<>>)) | Result], Options);
compile_impl([{'#', Key, Tags, Source} | T], Map, Result, Options) ->
    Value = data_get(convert_keytype(Key, Options), Map, false),
    case check_data_type(Value) of
        true                         -> compile_impl(T, Map, compile_impl(Tags, Value, Result, Options), Options);
        _ when is_list(Value)        -> compile_impl(T, Map, lists:foldl(fun(X, Acc) -> compile_impl(Tags, X, Acc, Options) end,
                                                                         Result, Value), Options);
        _ when Value =:= false       -> compile_impl(T, Map, Result, Options);
        _ when is_function(Value, 2) -> compile_impl(T, Map, [Value(Source, fun(Text) -> render(Text, Map, Options) end) | Result], Options);
        _                            -> compile_impl(T, Map, compile_impl(Tags, Map, Result, Options), Options)
    end;
compile_impl([{'^', Key, Tags} | T], Map, Result, Options) ->
    Value = data_get(convert_keytype(Key, Options), Map, false),
    case Value =:= [] orelse Value =:= false of
        true  -> compile_impl(T, Map, compile_impl(Tags, Map, Result, Options), Options);
        false -> compile_impl(T, Map, Result, Options)
    end;
compile_impl([Bin | T], Map, Result, Options) ->
    compile_impl(T, Map, [Bin | Result], Options).

%% @see parse_binary/1
-spec parse_binary_impl(state(), Input :: binary()) -> template().
parse_binary_impl(State, Input) ->
    #?MODULE{data = parse(State, Input)}.

%% @doc Analyze the syntax of the mustache.
-spec parse(state(), binary()) -> [tag()].
parse(State, Bin) ->
    case parse1(State, Bin, []) of
        {endtag, {_, OtherTag, _, _, _}} ->
            error({?PARSE_ERROR, {section_is_incorrect, OtherTag}});
        {_, Tags} ->
            lists:reverse(Tags)
    end.

%% @doc Part of the `parse/1'
%%
%% ATTENTION: The result is a list that is inverted.
-spec parse1(state(), Input :: binary(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse1(#state{start = Start, stop = Stop} = State, Bin, Result) ->
    case binary:split(Bin, Start) of
        [B1]                     -> {State, [B1 | Result]};
        [B1, <<"{", B2/binary>>] -> parse2(State, binary:split(B2, <<"}", Stop/binary>>), [B1 | Result]);
        [B1, B2]                 -> parse3(State, binary:split(B2, Stop),                 [B1 | Result])
    end.

%% @doc Part of the `parse/1'
%%
%% ATTENTION: The result is a list that is inverted.
-spec parse4(state(), Input :: binary(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse4(State, <<"\r\n", Rest/binary>>, Result) ->
    parse1(State, Rest, Result);
parse4(State, <<"\n", Rest/binary>>, Result) ->
    parse1(State, Rest, Result);
parse4(State, Input, Result) ->
    parse1(State, Input, Result).

%% @doc Part of the `parse/1'
%%
%% 2nd Argument: [TagBinary(may exist unnecessary spaces to the end), RestBinary]
%% ATTENTION: The result is a list that is inverted.
-spec parse2(state(), iolist(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse2(State, [B1, B2], Result) ->
    parse1(State, B2, [{'&', remove_space_from_edge(B1)} | Result]);
parse2(_, _, _) ->
    error({?PARSE_ERROR, unclosed_tag}).

%% @doc Part of the `parse/1'
%%
%% 2nd Argument: [TagBinary(may exist unnecessary spaces to the end), RestBinary]
%% ATTENTION: The result is a list that is inverted.
-spec parse3(state(), iolist(), Result :: [tag()]) -> {state(), [tag()]} | endtag().
parse3(State, [B1, B2], Result) ->
    case remove_space_from_head(B1) of
        <<"&", Tag/binary>> ->
            parse1(State, B2, [{'&', remove_space_from_edge(Tag)} | Result]);
        <<T, Tag/binary>> when T =:= $#; T =:= $^ ->
            parse_loop(State, ?COND(T =:= $#, '#', '^'), remove_space_from_edge(Tag), B2, Result);
        <<"=", Tag0/binary>> ->
            Tag1 = remove_space_from_tail(Tag0),
            Size = byte_size(Tag1) - 1,
            case Size >= 0 andalso Tag1 of
                <<Tag2:Size/binary, "=">> -> parse_delimiter(State, Tag2, B2, Result);
                _                         -> error({?PARSE_ERROR, {unsupported_tag, <<"=", Tag0/binary>>}})
            end;
        <<"!", _/binary>> ->
            parse1(State, B2, Result);
        <<"/", Tag/binary>> ->
            {endtag, {State, remove_space_from_edge(Tag), byte_size(B1) + 4, B2, Result}};
        <<">", Tag/binary>> ->
            parse_jump(State, remove_space_from_edge(Tag), B2, Result);
        Tag ->
            parse1(State, B2, [{n, remove_space_from_tail(Tag)} | Result])
    end;
parse3(_, _, _) ->
    error({?PARSE_ERROR, unclosed_tag}).

%% @doc Loop processing part of the `parse/1'
%%
%% `{{# Tag}}' or `{{^ Tag}}' corresponds to this.
-spec parse_loop(state(), '#' | '^', Tag :: binary(), Input :: binary(), Result :: [tag()]) -> [tag()] | endtag().
parse_loop(State0, Mark, Tag, Input, Result0) ->
    case parse4(State0, Input, []) of
        {endtag, {State, Tag, LastTagSize, Rest, Result1}} ->
            case Mark of
                '#' -> Source = binary:part(Input, 0, byte_size(Input) - byte_size(Rest) - LastTagSize),
                       parse4(State, Rest, [{'#', Tag, lists:reverse(Result1), Source} | Result0]);
                '^' -> parse4(State, Rest, [{'^', Tag, lists:reverse(Result1)} | Result0])
            end;
        {endtag, {_, OtherTag, _, _, _}} ->
            error({?PARSE_ERROR, {section_is_incorrect, OtherTag}});
        _ ->
            error({?PARSE_ERROR, {section_end_tag_not_found, <<"/", Tag/binary>>}})
    end.

%% @doc Endtag part of the `parse/1'
-spec parse_jump(state(), Tag :: binary(), NextBin :: binary(), Result :: [tag()]) -> [tag()] | endtag().
parse_jump(#state{dirname = Dirname} = State0, Tag, NextBin, Result0) ->
    Filename0 = <<Tag/binary, ".mustache">>,
    Filename  = ?COND(Dirname =:= <<>>, Filename0, filename:join([Dirname, Filename0])),
    case file:read_file(Filename) of
        {ok, Bin} ->
            case parse4(State0, Bin, Result0) of
                {endtag, {_, Tag, _, _, _}} -> error({?PARSE_ERROR, {section_begin_tag_not_found, <<"#", Tag/binary>>}});
                {State, Result}             -> parse4(State, NextBin, Result)
            end;
        _ ->
            error(?FILE_ERROR, [Filename])
    end.

%% @doc Update delimiter part of the `parse/1'
%%
%% ParseDelimiterBin :: e.g. `{{=%% %%=}}' -> `%% %%'
-spec parse_delimiter(state(), ParseDelimiterBin :: binary(), NextBin :: binary(), Result :: [tag()]) -> [tag()] | endtag().
parse_delimiter(State0, ParseDelimiterBin, NextBin, Result) ->
    case binary:match(ParseDelimiterBin, <<"=">>) of
        nomatch ->
            case [X || X <- binary:split(ParseDelimiterBin, <<" ">>, [global]), X =/= <<>>] of
                [Start, Stop] -> parse4(State0#state{start = Start, stop = Stop}, NextBin, Result);
                _             -> error({?PARSE_ERROR, delimiters_may_not_contain_whitespaces})
            end;
        _ ->
            error({?PARSE_ERROR, delimiters_may_not_contain_equals})
    end.

%% @doc Remove the space from the edge.
-spec remove_space_from_edge(binary()) -> binary().
remove_space_from_edge(Bin) ->
    remove_space_from_tail(remove_space_from_head(Bin)).

%% @doc Remove the space from the head.
-spec remove_space_from_head(binary()) -> binary().
remove_space_from_head(<<" ", Rest/binary>>) -> remove_space_from_head(Rest);
remove_space_from_head(Bin)                  -> Bin.

%% @doc Remove the space from the tail.
-spec remove_space_from_tail(binary()) -> binary().
remove_space_from_tail(<<>>) -> <<>>;
remove_space_from_tail(Bin) ->
    PosList = binary:matches(Bin, <<" ">>),
    LastPos = remove_space_from_tail_impl(lists:reverse(PosList), byte_size(Bin)),
    binary:part(Bin, 0, LastPos).

%% @see remove_space_from_tail/1
-spec remove_space_from_tail_impl([{non_neg_integer(), pos_integer()}], non_neg_integer()) -> non_neg_integer().
remove_space_from_tail_impl([{X, Y} | T], Size) when Size =:= X + Y ->
    remove_space_from_tail_impl(T, X);
remove_space_from_tail_impl(_, Size) ->
    Size.

%% @doc term to iodata
-spec to_iodata(number() | binary() | string() | atom()) -> iodata().
to_iodata(Integer) when is_integer(Integer) ->
    list_to_binary(integer_to_list(Integer));
to_iodata(Float) when is_float(Float) ->
    io_lib:format("~p", [Float]);
to_iodata(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
to_iodata(X) ->
    X.

%% @doc HTML Escape
-spec escape(iodata()) -> binary().
escape(IoData) ->
    Bin = iolist_to_binary(IoData),
    << <<(escape_char(X))/binary>> || <<X:8>> <= Bin >>.

%% @see escape/1
-spec escape_char(0..16#FFFF) -> binary().
escape_char($<) -> <<"&lt;">>;
escape_char($>) -> <<"&gt;">>;
escape_char($&) -> <<"&amp;">>;
escape_char($") -> <<"&quot;">>;
escape_char($') -> <<"&apos;">>;
escape_char(C)  -> <<C:8>>.

%% @doc convert to {@link data_key/0} from binary.
-spec convert_keytype(binary(), [option()]) -> data_key().
convert_keytype(KeyBin, Options) ->
    case proplists:get_value(key_type, Options, string) of
        atom ->
            try binary_to_existing_atom(KeyBin, utf8) of
                Atom -> Atom
            catch
                _:_ -> <<" ">> % It is not always present in data/0
            end;
        string -> binary_to_list(KeyBin);
        binary -> KeyBin
    end.

%% @doc fetch the value of the specified key from {@link data/0}
-spec data_get(data_key(), data(), Default :: term()) -> term().
-ifdef(namespaced_types).
data_get(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
data_get(Key, AssocList, Default) ->
    proplists:get_value(Key, AssocList, Default).
-else.
data_get(Key, AssocList, Default) ->
    proplists:get_value(Key, AssocList, Default).
-endif.

%% @doc check whether the type of {@link data/0}
%%
%% maybe: There is also the possibility of iolist
-spec check_data_type(data() | term()) -> boolean() | maybe.
-ifdef(namespaced_types).
check_data_type([])           -> maybe;
check_data_type([{_, _} | _]) -> true;
check_data_type(Map)          -> is_map(Map).
-else.
check_data_type([])           -> maybe;
check_data_type([{_, _} | _]) -> true;
check_data_type(_)            -> false.
-endif.
