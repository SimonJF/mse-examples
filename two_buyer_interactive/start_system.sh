#!/usr/bin/env bash
erl -sname system@guadeloupe -config config/system -pa ../../monitored-session-erlang/ebin ebin/ -eval "application:start(two_buyer_dist)"
