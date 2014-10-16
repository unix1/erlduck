erlduck
=======

erlduck is a DuckDuckGo Instant Answers API client written in Erlang/OTP.

Summary
-------

In a nutshell, erlduck is an OTP application that sends HTTP requests to api.duckduckgo.com and returns the result.

In Erlang-ese: it's a simple OTP application that starts a simple_one_for_one supervisor and dynamically creates worker gen_servers for executing API calls.

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

      `erlduck:search(<<"DuckDuckGo">>).`

* you should see raw output
