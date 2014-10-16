PROJECT = erlduck

# Options

CT_SUITES = online

# Dependencies

DEPS = httpclient
dep_httpclient = git https://github.com/unix1/httpclient.git

# Standard targets

include erlang.mk
