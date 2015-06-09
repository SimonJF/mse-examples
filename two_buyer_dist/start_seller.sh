#!/usr/bin/env bash
erl -sname seller@sillyname-vm -config config/seller -pa ../../monitored-session-erlang/ebin/ ebin/
