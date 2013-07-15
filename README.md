erlduck
=======

erlduck is a DuckDuckGo Instant Answers API client written in Erlang/OTP.

Summary
-------

In a nutshell, erlduck is an OTP application that sends HTTP requests to api.duckduckgo.com and returns the result.

In Erlang-ese: it's a simple OTP application that starts a simple_one_for_one supervisor and dynamically creates worker gen_servers for executing API calls.

Installation
------------

* compile

      `rebar compile`

* generate release

      `cd rel`  
      `rebar create-node nodeid=erlduck`  
      `cd ..`  
      `rebar generate`

Play
----

* run (and get console)

      `sh rel/erlduck/bin/erlduck console`

* play

      `erlduck:search("DuckDuckGo").`

* you should see raw output

