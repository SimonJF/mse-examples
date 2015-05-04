-module(state_cell_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, state_cell_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _StateCell} = ssa_gen_server:start(state_cell, [], []),
  io:format("started server successfully~n"),
  {ok, _Client} = ssa_gen_server:start(client, [], []),
  %receive
  %  {_Pid, blah} -> {}
  %end,
  ok.
