-module(specified_handler_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, specified_handler).
-define(TEST_HANDLER, specified_handler_test_handler_mod).

load() ->
    application:ensure_all_started(trails),
    meck:new(?TEST_HANDLER, [non_strict]),
    ok.

unload() ->
    true = meck:validate(?TEST_HANDLER),
    meck:unload(?TEST_HANDLER),
    application:stop(trails),
    ok.

req() ->
    req(<<"GET">>, #{}).

% req(post, raw, Opts) ->
%     req(<<"POST">>, Opts);
% req(post, json, Body) ->
%     Data = jsx:encode(Body),
%     req(<<"POST">>, #{'_test_body' => Data, has_body => true});
% req(post, binary, Data) ->
%     req(<<"POST">>, #{'_test_body' => Data, has_body => true}).
%
req(get, QueryArgs) ->
    QS = uri_string:compose_query(QueryArgs),
    req(<<"GET">>, #{qs => QS});
req(Method, Opts) when is_binary(Method) and erlang:is_map(Opts) ->
    Ref = make_ref(),
    Req = #{
        pid => self(),
        path => <<"/path">>,
        has_body => false,
        method => Method,
        headers => #{<<"content-type">> => <<"application/json">>},
        qs => <<"">>,
        streamid => Ref
    },
    MergedReq = maps:merge(Req, Opts),
    MergedReq.

read_reply(#{streamid := StreamId}) ->
    receive
        {{_Pid, StreamId}, Msg} -> Msg
    after 10 -> error
    end.

http_get_test() ->
    load(),
    Metadata = #{
        <<"get">> => #{
            operationId => testOP,
            tags => [],
            description => "Gets",
            parameters => [],
            responses => #{
                200 => #{
                    description => <<"Test Response">>,
                    content => #{
                        'application/json' => #{
                            schema => #{
                                type => object,
                                description => <<"Goal events">>,
                                properties => #{
                                    <<"id">> => #{
                                        type => string,
                                        description => <<"Collection of feature usage counts">>
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    },
    Trails = [trails:trail(<<"/path">>, ?TEST_HANDLER, #{}, Metadata)],
    ok = trails:store(Trails),
    Ref = make_ref(),

    meck:expect(?TEST_HANDLER, trails, [], Trails),
    meck:expect(?TEST_HANDLER, handle_req, fun(R, _Params, _Body, _State) ->
        {R, 200, #{}, Ref}
    end),
    meck:expect(?TEST_HANDLER, post_req, ['_', '_'], ok),

    Req = req(),
    {ok, _Resp, NewState} = ?MUT:upgrade(Req, #{}, ?TEST_HANDLER, #{}),

    ?assertEqual(Ref, NewState),

    unload().

http_get_by_param_test() ->
    load(),
    Metadata = #{
        <<"get">> => #{
            operationId => testOP,
            tags => [],
            description => "Gets",
            parameters => [
                #{
                    name => id,
                    description =>
                        <<"ID of object">>,
                    in => query,
                    schema => #{
                        type => integer
                    },
                    required => true
                }
            ],
            responses => #{
                200 => #{
                    description => <<"Test Response">>,
                    content => #{
                        'application/json' => #{
                            schema => #{
                                type => object,
                                description => <<"Goal events">>,
                                properties => #{
                                    <<"id">> => #{
                                        type => string,
                                        description => <<"Collection of feature usage counts">>
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    },
    Trails = [trails:trail(<<"/path">>, ?TEST_HANDLER, #{}, Metadata)],
    trails:store(Trails),
    Ref = make_ref(),

    meck:expect(?TEST_HANDLER, trails, [], Trails),
    meck:expect(?TEST_HANDLER, handle_req, fun(R, Params, Body, State) ->
        io:format("Req ~p~n", [{R, Params, Body, State}]),
        {R, 200, #{}, Ref}
    end),
    meck:expect(?TEST_HANDLER, post_req, ['_', '_'], ok),

    Req = req(get, [{<<"id">>, <<"1">>}]),

    io:format("REQ ~p~n", [Req]),
    {ok, _Resp, NewState} = ?MUT:upgrade(Req, #{}, ?TEST_HANDLER, #{}),

    ?assertEqual(Ref, NewState),

    unload().

http_get_by_param_incorrect_type_int_test() ->
    load(),
    Metadata = #{
        <<"get">> => #{
            operationId => testOP,
            tags => [],
            description => "Gets",
            parameters => [
                #{
                    name => id,
                    description =>
                        <<"ID of object">>,
                    in => query,
                    schema => #{
                        type => integer
                    },
                    required => true
                }
            ],
            responses => #{
                200 => #{
                    description => <<"Test Response">>,
                    content => #{
                        'application/json' => #{
                            schema => #{
                                type => object,
                                description => <<"Goal events">>,
                                properties => #{
                                    <<"id">> => #{
                                        type => string,
                                        description => <<"Collection of feature usage counts">>
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    },
    Trails = [trails:trail(<<"/path">>, ?TEST_HANDLER, #{}, Metadata)],
    trails:store(Trails),
    Ref = make_ref(),

    meck:expect(?TEST_HANDLER, trails, [], Trails),
    meck:expect(?TEST_HANDLER, handle_req, fun(R, _Params, _Body, _State) ->
        {R, 200, #{}, Ref}
    end),
    meck:expect(?TEST_HANDLER, post_req, ['_', '_'], ok),

    Req = req(get, [{<<"id">>, <<"a">>}]),

    io:format("REQ ~p~n", [Req]),
    {ok, Resp, _NewState} = ?MUT:upgrade(Req, #{}, ?TEST_HANDLER, #{}),
    {response, 400, _Headers, Body} = read_reply(Resp),

    RespData = jsx:decode(Body, [return_maps]),

    Expected = #{
        <<"error">> => #{
            <<"type_expected">> => <<"integer">>,
            <<"value">> => <<"a">>,
            <<"what">> => <<"Incorrect type">>
        }
    },
    ?assertEqual(Expected, RespData),

    unload().
