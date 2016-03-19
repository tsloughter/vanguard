-module(vanguard_cluster).

-behaviour(gen_server).

%% API
-export([start_link/0, add_nodes/0, create/0, create_ensembles/1, update_ensembles/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ENSEMBLES, [zones]).

-record(state, {nodes=[]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_nodes() ->
    gen_server:call(?SERVER, add_nodes, 20000).

create() ->
    gen_server:call(?SERVER, create, 20000).

init([]) ->
    {ok, #state{nodes=[node()]}}.

handle_call(add_nodes, _From, State) ->
    join_cluster(nodes()),
    {reply, ok, State};
handle_call(create, _From, State) ->
    riak_ensemble_manager:enable(),
    wait_stable(),
    create_ensembles(?ENSEMBLES),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

wait_stable() ->
    case check_stable() of
        true ->
            ok;
        false ->
            wait_stable()
    end.

check_stable() ->
    case riak_ensemble_manager:check_quorum(root, 1000) of
        true ->
            case riak_ensemble_peer:stable_views(root, 1000) of
                {ok, true} ->
                    true;
                _ ->
                    false
            end;
        false ->
            false
    end.

join_cluster([]) ->
    ok;
join_cluster([H|T]) ->
    case riak_ensemble_manager:join(H, node()) of
        ok ->
            wait_stable(),
            update_ensembles(?ENSEMBLES),
            ok;
        already_enabled ->
            wait_stable(),
            update_ensembles(?ENSEMBLES);
        {error, same_node} ->
            ok;
        _ ->
            if
                H =:= node() ->
                    join_cluster(T);
                true ->
                    join_cluster([H])
            end
    end.

update_ensembles(Ensembles) ->
    [begin
         {_, Node} = Peer = riak_ensemble_manager:get_leader(EnsembleId),
         Pid = rpc:call(Node, riak_ensemble_manager, get_peer_pid, [EnsembleId, Peer]),
         riak_ensemble_peer:update_members(Pid, [{add, {EnsembleId, node()}}], 5000)
     end || EnsembleId <- Ensembles].

create_ensembles(Ensembles) ->
    [riak_ensemble_manager:create_ensemble(EnsembleId,
                                           {EnsembleId, node()},
                                           vanguard_ensemble_backend,
                                           []) || EnsembleId <- Ensembles].
