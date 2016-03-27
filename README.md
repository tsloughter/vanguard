vanguard
=====

Consul clone.

### Build and Run

```
$ rebar3 release
$ _build/default/rel/vanguard/bin/vanguard console
```

### Update and Query

```
$ curl -v -XPUT localhost:8080/node/register -d "{'node':'localhost','port':0,'service':{'id':'service1'}}"
$ dig -p8053 @127.0.0.1 serv71.service.c1.vanguard srv
;; ANSWER SECTION:
service1.service.c1.vanguard. 3600	IN	SRV	1 1 0 localhost.
```
