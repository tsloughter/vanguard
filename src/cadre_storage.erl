-module(cadre_storage).

%% API
-export([insert/2,
         delete/2,
         select/2]).

-spec insert(atom(), tuple()) -> ok | {error, Reason :: term()}.
insert(Table, Value)->
    case riak_ensemble_client:kover(node(), Table, element(1, Value), Value, 10000) of
        {ok, _} ->
            ok;
        Err ->
            lager:error("error=~p", [Err]),
            {error, Err}
    end.

-spec delete(atom(), term()) -> ok.
delete(Table, Key) ->
    case riak_ensemble_client:kover(node(), Table, Key, notfound, 10000) of
        {ok, _} ->
            ok;
        Err ->
            lager:error("error=~p", [Err]),
            {error, Err}
    end.

select(Table, Key) ->
    case riak_ensemble_client:kget(node(), Table, Key, 10000) of
        {ok, Obj} ->
            vanguard_ensemble_backend:obj_value(Obj);
        Err ->
            lager:error("error=~p", [Err]),
            notfound
    end.
