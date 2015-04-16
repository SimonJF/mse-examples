#!/usr/bin/env bash
erl -sname buyer2@sillyname-vm -config config/buyer2 -pa deps/monitored_session_erlang/ebin ebin/
