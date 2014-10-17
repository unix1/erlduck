PROJECT = erlduck

# Options

CT_SUITES = online

# Dependencies

DEPS = httpclient jsx
dep_httpclient = git https://github.com/unix1/httpclient.git
dep_jsx = git https://github.com/talentdeficit/jsx.git

# Standard targets

include erlang.mk
