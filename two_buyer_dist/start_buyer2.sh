#!/usr/bin/env bash
erl -sname buyer2@sillyname-vm -config config/buyer2 -pa ../../monitored-session-erlang/ebin/ ebin/
