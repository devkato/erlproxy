Simple Load Balancer for Layer 4 by Erlang.

## TODO

* health check of backends and fail-over automatically.
* multiple algorithm for choosing a backend server.
* by weight, response time, number of requests served, etc...

## To Start

./rebar compile
erl -config erlproxy
