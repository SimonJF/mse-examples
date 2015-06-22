-module(sup_two_buyer_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, sup_two_buyer_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _SellerPID} = seller_sup:start_link(),
  io:format("started supervised seller successfully~n"),
  {ok, _Buyer2Pid} = ssa_gen_server:start(sup_buyer2, [], []),
  io:format("started buyer2 successfully~n"),
  {ok, Buyer1Pid} = ssa_gen_server:start(sup_buyer1, [], []),
  io:format("started buyer1 successfully~n"),
  %receive
  %  {_Pid, blah} -> {}
  %end,
  user_loop(Buyer1Pid).

user_loop(Buyer1PID) ->
  io:format("Press to start a new session~n"),
  io:get_line(""),
  sup_buyer1:start_new_conversation(Buyer1PID),
  user_loop(Buyer1PID).
