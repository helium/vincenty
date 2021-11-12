-module(vincenty).

-export([load/0]).

-on_load(load/0).

-export([distance/2]).

-type coordinate() :: {float(), float()}.

-spec distance(C1 :: coordinate(), C2 :: coordinate()) -> {ok, float()} | {error, any()}.
distance(_C1, _C2) ->
    not_loaded(?LINE).

%% ==================================================================
%% NIF
%% ==================================================================

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.

