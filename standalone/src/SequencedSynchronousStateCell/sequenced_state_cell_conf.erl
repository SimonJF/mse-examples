-module(sequenced_state_cell_conf).
-export([config/0]).

config() ->
  [{sequenced_state_cell, [{"SynchronousSequencedStateCells",
                  ["StateCell1", "StateCell2", "StateCellRes"]}]},
   {sequenced_client, [{"SynchronousSequencedStateCells", ["Client"]}]}].
