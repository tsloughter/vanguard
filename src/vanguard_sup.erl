%%%-------------------------------------------------------------------
%% @doc vanguard top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vanguard_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    DataRoot = application:get_env(riak_ensemble, data_root, "./data"),

    HttpInterface = {http_interface, {elli, start_link, [[{callback, vanguard_http_callback}, {port, 8080}]]},
                     permanent, 20000, worker, [elli]},
    Ensemble = {riak_ensemble_sup, {riak_ensemble_sup, start_link, [filename:join(DataRoot, atom_to_list(node()))]},
                permanent, 20000, supervisor, [riak_ensemble_sup]},
    Cluster = {vanguard_cluster, {vanguard_cluster, start_link, []},
                permanent, 20000, worker, [vanguard_cluster]},

    {ok, {{one_for_all, 0, 1}, [HttpInterface, Ensemble, Cluster]}}.

%%====================================================================
%% Internal functions
%%====================================================================
