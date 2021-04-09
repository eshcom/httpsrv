#!/bin/sh

./rebar.sh

erl -pa "./ebin" -detached -config httpsrv.config -s httpsrv
