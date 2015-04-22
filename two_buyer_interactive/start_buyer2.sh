#!/usr/bin/env bash
erl -sname buyer2@guadeloupe -config config/buyer2 -pa deps/monitored_session_erlang/ebin ebin/
