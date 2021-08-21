-module(specified_handler_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, specified_handler).
-define(TEST_HANDLER, specified_handler_test_handler_mod).

load() ->
    meck:new(?TEST_HANDLER, [non_strict]),
    ok.

unload() ->
    true = meck:validate(?TEST_HANDLER),
    meck:unload(?TEST_HANDLER),
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
        has_body => false,
        method => Method,
        headers => #{<<"content-type">> => <<"application/json">>},
        qs => <<"">>,
        streamid => Ref
    },
    MergedReq = maps:merge(Req, Opts),
    MergedReq.

minimal_http_get_test() ->
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
    Trails = [trails:trail("/path", ?TEST_HANDLER, #{}, Metadata)],
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
