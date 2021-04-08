#!/bin/sh

./rebar.sh

erl -pa "./ebin" -config httpsrv.config -s httpsrv
