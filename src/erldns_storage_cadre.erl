-module(erldns_storage_cadre).

%% API
-export([create/1,
         insert/2,
         delete_table/1,
         delete/2,
         backup_table/1,
         backup_tables/0,
         select/2,
         select/3,
         foldl/3,
         empty_table/1,
         list_table/1]).

-spec create(atom()) -> ok | not_implemented | {error, Reason :: term()}.
create(schema) ->
    not_implemented;
create(_Name) ->
    io:format("CREATE ~p~n", [_Name]),
    lager:info("CREATE ~p", [_Name]),
    ok.


%% @doc Insert value in ets table.
-spec insert(atom(), tuple()) -> ok | {error, Reason :: term()}.
insert(Table, Value)->
    io:format("INSERT ~p ~p~n", [Table, Value]),
    lager:info("INSERT ~p ~p", [Table, Value]),
    case riak_ensemble_client:kover(node(), Table, element(1, Value), Value, 10000) of
        {ok, _} ->
            ok;
        Err ->
            {error, Err}
    end.

%% @doc Delete entire ets table.
-spec delete_table(atom()) -> ok | {error, Reason :: term()}.
delete_table(_Table)->
    {error, not_implemented}.


-spec delete(atom(), term()) -> ok.
delete(_Table, Key) ->
    case riak_ensemble_client:kover(node(), root, Key, notfound, 10000) of
        {ok, _} ->
            ok;
        Err ->
            {error, Err}
    end.

%% @doc Backup a specific ets table.
%% @see https://github.com/SiftLogic/erl-dns/issues/3
-spec backup_table(atom()) -> ok | {error, Reason :: term()}.
backup_table(_Table)->
    {error, not_implemented}.

%% @doc Should backup all ets tables.
%% @see https://github.com/SiftLogic/erl-dns/issues/3
-spec backup_tables() -> ok | {error, Reason :: term()}.
backup_tables() ->
    {error, not_implemented}.

select(Table, Key) ->
    case riak_ensemble_client:kget(node(), Table, Key, 10000) of
        {ok,Obj} ->
            case vanguard_ensemble_backend:obj_value(Obj) of
                notfound ->
                    [];
                Value ->
                    [Value]
            end;
        Err ->
            {error, Err}
    end.

%% #doc Select from ets using match specs.
-spec select(atom(), list(), integer() | infinite) -> tuple() | '$end_of_table'.
select(_Table, _MatchSpec, infinite) ->
    {error, not_implemented};
select(_Table, _MatchSpec, _Limit) ->
    {error, not_implemented}.

%% @doc Wrapper for foldl in ets.
-spec foldl(fun(), list(), atom())  -> Acc :: term() | {error, Reason :: term()}.
foldl(_Fun, _Acc, _Table) ->
    {error, not_implemented}.

%% @doc Empty ets table. Ets always returns true for this function.
-spec empty_table(atom()) -> ok.
empty_table(_Table) ->
    ok.

%% @doc Lists the ets table
-spec list_table(atom()) -> term() | {error, term()}.
list_table(_TableName) ->
    {error, not_implemented}.
