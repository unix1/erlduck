erlduck
=======

erlduck is a DuckDuckGo Instant Answers API client written in Erlang/OTP.

Summary
-------

In a nutshell, erlduck is an HTTP client application that sends HTTP requests
to api.duckduckgo.com and returns the result. It uses unix1/httpclient using
extend/gun as its HTTP backend. This means erlduck will keep configured
number of workers (default 2) persistently connected to DuckDuckGo service
for fast response times.

Installation
------------

* compile and make release

      `make`

* run tests (assumes connectivity to api.duckduckgo.com)

      `make tests`

Play
----

* run (and get console)

      `_rel/erlduck_release/bin/erlduck_release console`

* play

      `erlduck:get_answer(<<"convert 5 inches to cm">>).`

* to see the raw undecoded json response

      `erlduck:search(<<"snakes on a plane">>).`
