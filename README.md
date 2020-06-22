# erlduck

[![unix1](https://img.shields.io/circleci/project/unix1/erlduck)](https://circleci.com/gh/unix1/erlduck)
[![unix1](https://img.shields.io/github/license/unix1/erlduck)](https://apache.org/licenses/LICENSE-2.0.html)

erlduck is a DuckDuckGo Instant Answers API client written in Erlang/OTP.

## Summary

In a nutshell, erlduck is an HTTP client application that sends HTTP requests
to api.duckduckgo.com and returns the result. It uses unix1/httpclient using
ninenines/gun as its HTTP backend. This means erlduck will keep configured
number of workers (default 1) persistently connected to DuckDuckGo service
for fast response times.

## Prerequisites

* Erlang/OTP

## Run

* run in shell

```
make run
```

* run tests (assumes connectivity to api.duckduckgo.com)

```
make tests
```

## Play

* after running in shell, you can start the client pool

```erlang
erlduck:start().
```

* and then make requests using that pool

```erlang
erlduck:get_answer(<<"what is erlang">>).
```

* to see the raw undecoded json response

```erlang
erlduck:search(<<"snakes on a plane">>).
```

* to start with custom options (map may contain one more keys below):

```erlang
erlduck:start(#{name => other, pool_size => 2, pool_overflow => 2}).
```

* and then use the custom pool for requests

```erlang
erlduck:get_answer(other, <<"snakes on a plane">>).
```
