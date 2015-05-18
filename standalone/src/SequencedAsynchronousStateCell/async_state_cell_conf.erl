-module(async_state_cell_conf).
-export([config/0]).

config() ->
  [{async_state_cell, [{"SequencedStateCells",
                  ["StateCell1", "StateCell2", "StateCellRes"]}]},
   {async_client, [{"SequencedStateCells", ["Client"]}]}].
