-module(vanguard_ensemble_backend).
-behaviour(riak_ensemble_backend).

-export([init/3, new_obj/4]).
-export([obj_epoch/1, obj_seq/1, obj_key/1, obj_value/1]).
-export([set_obj_epoch/2, set_obj_seq/2, set_obj_value/2]).
-export([get/3, put/4, tick/5, ping/2, ready_to_start/0]).
-export([synctree_path/2]).
-export([handle_down/4]).

-include_lib("riak_ensemble/include/riak_ensemble_types.hrl").

-record(obj, {epoch :: epoch(),
              seq   :: seq(),
              key   :: term(),
              value :: term()}).

-record(state, {savefile :: file:filename(),
                id       :: peer_id(),
                tid     :: ets:tid()}).

-type obj()   :: #obj{}.
-type state() :: #state{}.
-type key()   :: any().
-type value() :: any().

%%===================================================================

-spec init(ensemble_id(), peer_id(), []) -> state().
init(Ensemble, Id, []) ->
    %% TODO: Any concerns about using hash here?
    %% TODO: For root ensemble, should we use different naming scheme?
    <<Hash:160/integer>> = riak_ensemble_util:sha(term_to_binary({Ensemble, Id})),
    Name = integer_to_list(Hash),
    {ok, Root} = application:get_env(riak_ensemble, data_root),
    File = filename:join([Root, "ensembles", Name ++ "_kv"]),
    Tid = reload_data(File),
    #state{savefile=File, tid=Tid, id=Id}.

%%===================================================================

-spec new_obj(epoch(), seq(), key(), value()) -> obj().
new_obj(Epoch, Seq, Key, Value) ->
    #obj{epoch=Epoch, seq=Seq, key=Key, value=Value}.

%%===================================================================

-spec obj_epoch(obj()) -> epoch().
obj_epoch(Obj) ->
    Obj#obj.epoch.

-spec obj_seq(obj()) -> seq().
obj_seq(Obj) ->
    Obj#obj.seq.

-spec obj_key(obj()) -> key().
obj_key(Obj) ->
    Obj#obj.key.

-spec obj_value(obj()) -> value().
obj_value(Obj) ->
    Obj#obj.value.

%%===================================================================

-spec set_obj_epoch(epoch(), obj()) -> obj().
set_obj_epoch(Epoch, Obj) ->
    Obj#obj{epoch=Epoch}.

-spec set_obj_seq(seq(), obj()) -> obj().
set_obj_seq(Seq, Obj) ->
    Obj#obj{seq=Seq}.

-spec set_obj_value(value(), obj()) -> obj().
set_obj_value(Value, Obj) ->
    Obj#obj{value=Value}.

%%===================================================================

-spec get(key(), riak_ensemble_backend:from(), state()) -> state().
get(Key, From, State=#state{tid=Tid}) ->
    Reply = case ets:lookup(Tid, Key) of
                [Value] ->
                    element(2, Value);
                [] ->
                    notfound
            end,
    riak_ensemble_backend:reply(From, Reply),
    State.

-spec put(key(), obj(), riak_ensemble_backend:from(), state()) -> state().
put(Key, Obj, From, State=#state{savefile=File, tid=Tid}) ->
    ets:insert(Tid, {Key, Obj}),
    save_data(File, Tid),
    riak_ensemble_backend:reply(From, Obj),
    State.

%%===================================================================

-spec tick(epoch(), seq(), peer_id(), views(), state()) -> state().
tick(_Epoch, _Seq, _Leader, _Views, State) ->
    State.

-spec ping(pid(), state()) -> {ok, state()}.
ping(_From, State) ->
    {ok, State}.

ready_to_start() ->
    true.

synctree_path(_Ensemble, _Id) ->
    default.

%%===================================================================

-spec handle_down(reference(), pid(), term(), state()) -> false.
handle_down(_Ref, _Pid, _Reason, _State) ->
    false.

%%===================================================================

-spec reload_data(file:filename()) -> ets:tid().
reload_data(File) ->
    case load_saved_data(File) of
        {ok, Tid} ->
            Tid;
        not_found ->
            ets:new(cadre, [ordered_set, private, {read_concurrency, true}])
    end.

-spec load_saved_data(file:filename()) -> not_found | {ok, ets:tid()}.
load_saved_data(File) ->
    case filelib:is_regular(File) of
        true ->
            ets:file2tab(File, [{verify, true}]);
        _ ->
            not_found
    end.


-spec save_data(file:filename(), ets:tid()) -> ok.
save_data(File, Tid) ->
    ok = filelib:ensure_dir(File),
    ets:tab2file(Tid, File, [{extended_info, [md5sum, object_count]}, {sync, true}]).
