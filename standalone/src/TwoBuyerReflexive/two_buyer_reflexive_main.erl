-module(two_buyer_reflexive_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, two_buyer_reflexive_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _ServerPid} = ssa_gen_server:start(reflexive_seller, [], []),
  io:format("started server successfully~n"),
  {ok, _Buyers} = ssa_gen_server:start(reflexive_buyer, [], []),
  io:format("started buyers successfully~n"),
  %receive
  %  {_Pid, blah} -> {}
  %end,
  ok.
