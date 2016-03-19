-module(vanguard_console).

-export([join/1,
         create/1,
         leave/1,
         status/1,
         cluster_info/1,
         cluster_status/0]).

-export([ensemble_status/1]).

join([NodeStr]) ->
    case net_adm:ping(list_to_atom(NodeStr)) of
        pang ->
            {error, not_reachable};
        pong ->
            vanguard_cluster:add_nodes()
    end.

create([]) ->
    vanguard_cluster:create().

leave([]) ->
    ok.

status([]) ->
    ok.

cluster_info([]) ->
    ok.

ensemble_status([]) ->
    ok.

cluster_status() ->
    case riak_ensemble_manager:enabled() of
        false ->
            {error, not_enabled};
        true ->
            Nodes = lists:sort(riak_ensemble_manager:cluster()),
            io:format("Nodes in cluster: ~p~n",[Nodes]),
            LeaderNode = node(riak_ensemble_manager:get_leader_pid(root)),
            io:format("Leader: ~p~n",[LeaderNode])
    end.
