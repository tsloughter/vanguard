-module(cadre).

-export([register/3,
         find/1]).

-include_lib("dns/include/dns_records.hrl").

%% service: [tag.]<service>.service.<cluster>.<domain>

register(ServiceId, Node, Port) ->
    cadre_storage:insert(services, {ServiceId, Node, Port}).

find(ServiceId) ->
    cadre_storage:select(services, ServiceId).
