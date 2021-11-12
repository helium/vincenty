-module(vincenty_test).

-include_lib("eunit/include/eunit.hrl").

identity_test() ->
	{ok, Ans} = vincenty:distance({0.0, 0.0}, {0.0, 0.0}),
    ?assertEqual(Ans, 0.0).

basic_test() ->
	{ok, Ans} = vincenty:distance({42.3541165, -71.0693514}, {40.7791472, -73.9680804}),
    ?assertEqual(Ans, 298.396186).

known_test() ->
	{ok, Ans} = vincenty:distance({39.152501,-84.412977}, {39.152505,-84.412946}),
    ?assertEqual(Ans, 0.002716).

%% big_test() ->
%%     lists:foreach(fun(_) ->
%%                           {ok, _Ans} = vincenty:distance({39.152501,-84.412977}, {39.152505,-84.412946})
%%                   end, lists:seq(1, 10000)),
%%     ok.
