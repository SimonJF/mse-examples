-module(async_state_cell_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the sequenced state cell example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, async_state_cell_conf:config()),
  {ok, _StateCell2PID} = ssa_gen_server:start(async_state_cell, [10], []),
  {ok, _StateCell1PID} = ssa_gen_server:start(async_state_cell, [20], []),
  {ok, _StateCellResPID} = ssa_gen_server:start(async_state_cell, [0], []),
  {ok, _ClientPID} = ssa_gen_server:start(async_client, [], []),
  ok.
