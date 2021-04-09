#!/bin/sh

erl -pa "./ebin" -s httpcli_dispatch start localhost 9000 100 1 big
