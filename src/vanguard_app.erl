%%%-------------------------------------------------------------------
%% @doc vanguard public API
%% @end
%%%-------------------------------------------------------------------

-module(vanguard_app).

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

-include_lib("dns/include/dns_records.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    initialize_domain(),
    vanguard_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

initialize_domain() ->
    {ok, Domain} = application:get_env(vanguard, domain),
    {ok, Cluster} = application:get_env(vanguard, cluster),
    Zone = <<"service.", (list_to_binary(Cluster))/binary, ".", (list_to_binary(Domain))/binary>>,
    SOA = #dns_rrdata_soa{mname = <<"ns1.", Zone/binary>>,
                          rname = <<"admin.", Zone/binary>>,
                          serial = 2013022001,
                          refresh = 86400,
                          retry = 7200,
                          expire = 604800,
                          minimum = 300},
    erldns_zone_cache:put_zone({Zone, [], [#dns_rr{name = Zone,
                                                   type = ?DNS_TYPE_SOA,
                                                   ttl = 3600,
                                                   data = SOA}]}).
