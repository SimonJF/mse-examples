-module(deadlock_example_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, deadlock_example_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _ServerPid} = ssa_gen_server:start(actor1, [], []),
  io:format("started actor 1 successfully~n"),
  {ok, _Buyers} = ssa_gen_server:start(actor2, [], []),
  io:format("started actor 2 successfully~n"),
  ok.
