-module(travel_agent_main).
-compile(export_all).
-define(SPEC_DIR, "scribble_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, travel_agent_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _TAPID} = ssa_gen_server:start(travel_agent, [], []),
  io:format("started travel agent successfully~n"),
  {ok, _PPSPID} = ssa_gen_server:start(payment_processing_service, [], []),
  io:format("started payment processing service successfully~n"),
  {ok, _HBSPID} = ssa_gen_server:start(hotel_booking_service, [], []),
  io:format("started hotel booking service successfully~n"),
  {ok, _FBSPID} = ssa_gen_server:start(flight_booking_service, [], []),
  io:format("started flight booking service successfully~n"),
  {ok, _CPID} = ssa_gen_server:start(travel_customer, [], []),
  io:format("started customer successfully~n"),
  ok.
