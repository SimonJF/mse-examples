#!/usr/bin/env bash
erl -sname buyer1@guadeloupe -config config/buyer1 -pa deps/monitored_session_erlang/ebin ebin/
