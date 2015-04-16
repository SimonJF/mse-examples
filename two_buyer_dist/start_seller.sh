#!/usr/bin/env bash
erl -sname seller@sillyname-vm -config config/seller -pa deps/monitored_session_erlang/ebin ebin/
