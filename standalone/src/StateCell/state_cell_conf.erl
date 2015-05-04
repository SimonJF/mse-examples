-module(state_cell_conf).
-export([config/0]).

config() ->
  [{state_cell, [{"StateCell", ["StateCell"]}]},
   {client, [{"StateCell", ["Client"]}]}].
