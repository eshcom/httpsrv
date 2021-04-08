#!/bin/sh

#~ ./clean.sh
rm -fR src/*.beam

rebar -C rebar.config compile
