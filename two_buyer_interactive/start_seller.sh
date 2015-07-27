#!/usr/bin/env bash
erl -sname seller@guadeloupe -config config/seller -pa ../../monitored-session-erlang/ebin ebin/
