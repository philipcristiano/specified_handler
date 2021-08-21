-module(specified_handler).

-include_lib("kernel/include/logger.hrl").

-export([upgrade/4]).

-export([
    method_metadata/2,
    response_spec/2,
    response_spec/3
]).

-callback handle_req(cowboy_req:req(), any()) ->
    {cowboy_req:req(), integer(), any(), cowboy_http:opts()}.

upgrade(Req = #{method := Method}, _Env, Handler, HandlerState) ->
    Response =
        try
            Spec = method_metadata(Handler, Method),
            handle_req(Req, Spec, Handler, HandlerState)
        of
            {Req1, Code, Data, State} ->
                respond(Req1, Code, Data, State);
            {Req1, Code, Headers, Data, State} ->
                respond(Req1, Code, Headers, Data, State)
        catch
            {missing_required_key, Key} ->
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => <<"Missing required element">>,
                            key => Key
                        }
                    },
                    []
                );
            {method_not_defined, ErrorMethod} ->
                respond(
                    Req,
                    405,
                    #{
                        error => #{
                            what => <<"Method not allowed">>,
                            method => ErrorMethod
                        }
                    },
                    []
                );
            {invalid_feature, Message} ->
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => <<"Invalid feature">>,
                            description => ensure_binary(Message)
                        }
                    },
                    []
                );
            {invalid_date, Value} ->
                Msg = <<"Date doesn't appear to be the right format">>,
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => Msg,
                            value => ensure_binary(Value)
                        }
                    },
                    []
                );
            {invalid_enum, Value, Enum} ->
                Msg = <<"Value not in enum">>,
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => Msg,
                            choices => Enum,
                            value => ensure_binary(Value)
                        }
                    },
                    []
                );
            {invalid_json, Object} ->
                Msg = <<"The object is not valid JSON">>,
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => Msg,
                            object => Object
                        }
                    },
                    []
                );
            {invalid_contenttype, Type, Types} ->
                Msg = <<"The content-type is invalid">>,
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => Msg,
                            type => Type,
                            expected_types => Types
                        }
                    },
                    []
                );
            {invalid_base64, Object} ->
                Msg = <<"The object cannot be base64 decoded">>,
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => Msg,
                            object => Object
                        }
                    },
                    []
                );
            {incorrect_type, Value, Type} ->
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => <<"Incorrect type">>,
                            type_expected => Type,
                            value => Value
                        }
                    },
                    []
                );
            {matches_more_than_oneOf, Object} ->
                Msg = <<"Object matches more than oneOf specifications">>,
                respond(
                    Req,
                    400,
                    #{error => #{what => Msg, object => Object}},
                    []
                );
            {not_any_oneOf, Object, Whys} ->
                Msg = <<"Object does not match any of the oneOf specifications">>,
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => Msg,
                            object => Object,
                            why => tuples_to_lists(Whys)
                        }
                    },
                    []
                );
            {not_any_of, Object, Whys} ->
                Msg = <<"The object did not match anyOf">>,
                respond(
                    Req,
                    400,
                    #{
                        error => #{
                            what => Msg,
                            object => Object,
                            why => tuples_to_lists(Whys)
                        }
                    },
                    []
                )
        end,
    {ok, RespReq, NewHandlerState} = Response,

    try Handler:post_req(RespReq, NewHandlerState) of
        _ -> ok
    catch
        ErrorType:Any:Stacktrace ->
            ?LOG_ERROR(#{
                what => post_req_error,
                type => ErrorType,
                stack => Stacktrace,
                error => Any
            })
    end,

    Response.

handle_req(Req, Spec, Handler, HandlerState) ->
    Params = params_from_request(Req, Spec),
    {Req1, BodyData} = body_from_request(Req, Spec),
    Handler:handle_req(
        Req1,
        Params,
        BodyData,
        HandlerState
    ).

respond(Req, Code = 204, _Value, Opts) ->
    % 204 can't have a body, do this to avoid trying to jsx encode
    respond(Req, Code, #{}, <<"">>, Opts);
respond(Req, Code, Value, Opts) ->
    respond(Req, Code, #{}, Value, Opts).

respond(Req, Code = 204, Headers, _Value, Opts) when is_map(Headers) ->
    Req1 = cowboy_req:reply(Code, Headers, <<"">>, Req),
    {ok, Req1, Opts};
respond(Req, Code, Headers, Value, Opts) when is_map(Headers) ->
    Data = jsx:encode(Value),
    Req1 = cowboy_req:reply(
        Code,
        Headers#{
            <<"content-type">> => <<"application/json">>
        },
        Data,
        Req
    ),
    {ok, Req1, Opts}.

%%%%
%   Internal
%%%%

assert_has_keys([], _Map) ->
    ok;
assert_has_keys([H | T], Map) ->
    ?LOG_DEBUG(#{
        what => <<"assert has keys">>,
        key => H,
        map => Map
    }),
    case maps:get(H, Map, undefined) of
        undefined -> throw({missing_required_key, H});
        _ -> ok
    end,
    assert_has_keys(T, Map),
    ok.

match_params(_Params = [], _Req) ->
    [];
match_params(
    _Params = [
        _Spec = #{
            name := Name,
            in := query,
            required := IsRequired,
            schema := #{
                type := Type
            } = Schema
        }
        | T
    ],
    Req
) ->
    % Pick the default value to use based on type
    DefaultLookupTable = #{
        string => {undefined, fun noop/1},
        integer => {undefined, fun int_or_incorrect_type/1},
        array => {[], fun wrap_list_if_needed/1}
    },
    #{Type := {DefaultDefault, PostFunc}} = DefaultLookupTable,
    DefaultValue = maps:get(default, Schema, DefaultDefault),

    Matcher =
        case IsRequired of
            false -> [{Name, [], DefaultValue}];
            true -> [{Name, [nonempty], undefined}]
        end,

    #{Name := CowboyValue} = cowboy_req:match_qs(Matcher, Req),
    Value = PostFunc(CowboyValue),

    case {IsRequired, Value} of
        {true, undefined} -> throw({missing_required_key, Name});
        _ -> ok
    end,

    Param = validate_property_spec(Value, Schema),
    [{Name, Param} | match_params(T, Req)].

match_schema(_Schema = #{oneOf := OneOfs}, Data) ->
    %  Match each of the OneOfs
    MapOfs = fun(O) ->
        try match_schema(O, Data) of
            Params -> {ok, Params}
        catch
            ValidationError -> {error, ValidationError}
        end
    end,
    OneOfParams = lists:map(MapOfs, OneOfs),

    % Split the matched params from the error params
    Matched = proplists:get_all_values(ok, OneOfParams),
    Errored = proplists:get_all_values(error, OneOfParams),

    case Matched of
        [OnlyMatch] -> OnlyMatch;
        [] -> throw({not_any_oneOf, Data, Errored});
        _ -> throw({matches_more_than_oneOf, Data})
    end;
match_schema(_Schema = #{items := ItemSpec}, Data) when is_list(Data) ->
    MatchItem = fun(Item) ->
        match_schema(ItemSpec, Item)
    end,
    lists:map(MatchItem, Data);
match_schema(_Schema = #{items := _Properties}, Data) ->
    % items means this is an array, but the data should have matched above
    % with is_list
    throw({incorrect_type, Data, array});
match_schema(Schema = #{properties := Properties}, Data) when is_map(Data) ->
    ?LOG_DEBUG(#{
        what => <<"Match schema">>,
        schema => Schema,
        data => Data
    }),

    Required = maps:get(required, Schema, []),
    Fun = fun(K, PropSpec, AccIn) ->
        % Data in from jsx will be binaries, not atoms
        KBin = erlang:atom_to_binary(K, utf8),
        DefaultValue = maps:get(default, PropSpec, undefined),
        DataValue = maps:get(KBin, Data, DefaultValue),
        ValidDataValue = validate_property_spec(DataValue, PropSpec),
        ok = validate_enum(ValidDataValue, PropSpec),
        maps:put(K, ValidDataValue, AccIn)
    end,

    Params = maps:fold(Fun, #{}, Properties),
    assert_has_keys(Required, Params),
    Params;
match_schema(_Schema = #{properties := _Properties}, Data) ->
    % properties means this is an object, but the data should have matched
    % above with is_map
    throw({incorrect_type, Data, object}).

validate_property_spec(undefined, _Spec) ->
    undefined;
validate_property_spec(Value, _Spec = #{type := boolean}) ->
    case Value of
        true -> Value;
        false -> Value;
        _ -> throw({incorrect_type, Value, boolean})
    end;
validate_property_spec(Value, _Spec = #{type := integer}) ->
    case erlang:is_integer(Value) of
        true -> Value;
        false -> throw({incorrect_type, Value, integer})
    end;
validate_property_spec(Value, _Spec = #{type := number}) ->
    case erlang:is_number(Value) of
        true -> Value;
        false -> throw({incorrect_type, Value, number})
    end;
validate_property_spec(Value, _Spec = #{type := string, format := 'date-time'}) ->
    case is_binary(Value) of
        false ->
            throw({incorrect_type, Value, string});
        _ ->
            StringValue = binary:bin_to_list(Value),
            try calendar:rfc3339_to_system_time(StringValue) of
                Date -> Date
            catch
                error:{badmatch, _DateValue} ->
                    throw({invalid_date, Value})
            end
    end;
validate_property_spec(Value, _Spec = #{type := string, format := byte}) ->
    case is_binary(Value) of
        true ->
            try base64:decode(Value) of
                Decoded -> Decoded
            catch
                error:badarg ->
                    throw({invalid_base64, Value})
            end;
        false ->
            throw({incorrect_type, Value, string})
    end;
validate_property_spec(Value, _Spec = #{type := string}) ->
    is_type_or_throw(Value, fun is_binary/1, string);
% Item list with anyOf definition
validate_property_spec(
    Value,
    _Spec = #{
        type := array,
        items := #{anyOf := AnyOf}
    }
) ->
    is_type_or_throw(Value, fun is_list/1, array),
    validate_any_of(Value, AnyOf);
% Item list with object definition
validate_property_spec(
    Value,
    _Spec = #{
        type := array,
        items := ItemSpec
    }
) ->
    is_type_or_throw(Value, fun is_list/1, array),
    case maps:get(type, ItemSpec) of
        object -> [match_schema(ItemSpec, V) || V <- Value];
        _ -> [validate_property_spec(V, ItemSpec) || V <- Value]
    end.

validate_enum(Value, #{enum := Enum}) ->
    case lists:member(Value, Enum) of
        true -> ok;
        false -> throw({invalid_enum, Value, Enum})
    end;
validate_enum(_Value, #{}) ->
    ok.

validate_any_of(Items, AnyOfs) ->
    validate_any_of(Items, AnyOfs, []).

% Value list exhausted, everything is validated
validate_any_of([], _Any, _Errors) ->
    [];
% anyOf list exhausted, this isn't anyOf
validate_any_of([Hv | _Tv], [], Errors) ->
    throw({not_any_of, Hv, Errors});
% Iterate values and compare to possible anyOfs
validate_any_of([Hv | Tv], [Ha = #{type := object} | Ta], Errors) ->
    Validated =
        try match_schema(Ha, Hv) of
            TryValidated -> TryValidated
        catch
            ValidationError ->
                [NextIsValid] = validate_any_of(
                    [Hv],
                    Ta,
                    [ValidationError | Errors]
                ),
                NextIsValid
        end,
    [Validated | validate_any_of(Tv, [Ha | Ta], [])].

method_metadata(Handler, Method) ->
    LowerMethod = string:lowercase(Method),
    [Trails] = Handler:trails(),
    Metadata = trails:metadata(Trails),
    case maps:is_key(LowerMethod, Metadata) of
        false -> throw({method_not_defined, LowerMethod});
        true -> maps:get(LowerMethod, Metadata)
    end.

params_from_request(Req = #{}, Spec) ->
    SpecParams = maps:get(parameters, Spec, []),
    Params = match_params(SpecParams, Req),
    Params.

body_from_request(
    _Req = #{has_body := false},
    _Spec = #{requestBody := #{required := true}}
) ->
    throw(body_required);
body_from_request(Req = #{has_body := false}, _Spec) ->
    {Req, undefined};
body_from_request(Req = #{has_body := true}, Spec) ->
    SpecBody = maps:get(requestBody, Spec),
    SpecContent = maps:get(content, SpecBody),
    ContentType = cowboy_req:header(<<"content-type">>, Req),
    ContentTypeAtom = erlang:binary_to_atom(ContentType, utf8),

    ContentSpec =
        case maps:is_key(ContentTypeAtom, SpecContent) of
            false ->
                Types = maps:keys(SpecContent),
                throw({invalid_contenttype, ContentTypeAtom, Types});
            true ->
                maps:get(ContentTypeAtom, SpecContent)
        end,

    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Data = decode_or_throw(Body, {invalid_json, Body}),
    #{schema := Schema} = ContentSpec,
    ParsedData = match_schema(Schema, Data),

    {Req1, ParsedData}.

response_spec(Spec, Code) ->
    Responses = maps:get(responses, Spec, #{}),
    CodeSpec = maps:get(Code, Responses),
    CodeSpec.

response_spec(Handler, Method, Code) ->
    Spec = method_metadata(Handler, Method),
    response_spec(Spec, Code).

ensure_binary(Bin) when is_binary(Bin) ->
    Bin;
ensure_binary(List) when is_list(List) ->
    binary:list_to_bin(List).

is_type_or_throw(Value, Predicate, Type) ->
    case Predicate(Value) of
        true -> Value;
        false -> throw({incorrect_type, Value, Type})
    end.

tuples_to_lists([]) ->
    [];
tuples_to_lists([H | T]) when is_tuple(H) ->
    NewH = [element(I, H) || I <- lists:seq(1, tuple_size(H))],
    [NewH | tuples_to_lists(T)].

noop(X) -> X.

wrap_list_if_needed(X) when is_list(X) -> X;
wrap_list_if_needed(X) -> [X].

decode_or_throw(Bin, Throw) ->
    case jsx:is_json(Bin) of
        true -> jsx:decode(Bin, [return_maps]);
        false -> throw(Throw)
    end.

int_or_incorrect_type(Value) ->
    io:format("Typecheck ~p~n", [Value]),
    try erlang:binary_to_integer(Value) of
        IntVal -> IntVal
    catch
        error:badarg -> throw({incorrect_type, Value, integer})
    end.
