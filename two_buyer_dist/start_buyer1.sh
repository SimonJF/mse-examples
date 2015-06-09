#!/usr/bin/env bash
erl -sname buyer1@sillyname-vm -config config/buyer1 -pa ../../monitored-session-erlang/ebin/ ebin/
