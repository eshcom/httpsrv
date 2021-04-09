#!/bin/sh

./rebar.sh

erl -pa "./ebin" -s httpcli_dispatch start localhost 9000 80 1 big
