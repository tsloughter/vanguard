-module(vanguard_http_callback).

-export([handle/2,
        handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('PUT',[<<"node">>, <<"register">>], Req) ->
    #{<<"node">> := Node,
      <<"port">> := Port,
      <<"service">> := #{<<"id">> := ServiceId}} = jsx:decode(elli_request:body(Req), [return_maps]),
    ok = cadre:register(ServiceId, Node, Port),
    {ok, [], <<"">>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.
