#!/usr/bin/env bash
erl -sname seller@guadeloupe -config config/seller -pa deps/monitored_session_erlang/ebin ebin/
