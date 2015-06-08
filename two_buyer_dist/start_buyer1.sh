#!/usr/bin/env bash
erl -sname buyer1@guadeloupe -config config/buyer1 -pa ../../monitored-session-erlang/ebin/ ebin/
