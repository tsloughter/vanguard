-module(vanguard_delegate).

-behaviour(erldns_resolver).

-export([get_records_by_name/1]).

-include_lib("dns/include/dns_records.hrl").

get_records_by_name(Qname) ->
    ServiceId = get_service_id(Qname),
    case cadre:find(ServiceId) of
        {_, Target, Port} ->
            [#dns_rr{name = Qname,
                    type = ?DNS_TYPE_SRV,
                    ttl  = 3600,
                    data = #dns_rrdata_srv{priority = 1,
                                           weight   = 1,
                                           port     = Port,
                                           target   = Target}}];
        notfound ->
            []
    end.

get_service_id(Qname) ->
    {ok, Domain0} = application:get_env(vanguard, domain),
    Domain = list_to_binary(Domain0),
    {ok, Cluster0} = application:get_env(vanguard, cluster),
    Cluster = list_to_binary(Cluster0),
    [ServiceId, <<"service">>, Cluster, Domain] = binary:split(Qname, <<".">>, [global]),
    ServiceId.
