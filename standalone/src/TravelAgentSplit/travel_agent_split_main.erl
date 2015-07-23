-module(travel_agent_split_main).
-compile(export_all).
-define(SPEC_DIR, "travel_agent_split_specs").

% Main entry point for the two-buyer protocol example.
main() ->
  % Initialise the conversation runtime.
  conversation:initialise(?SPEC_DIR, travel_agent_split_conf:config()),
  io:format("Initialised successfully~n"),
  {ok, _TAPID} = ssa_gen_server:start(travel_agent_split, [], []),
  io:format("started travel agent successfully~n"),
  {ok, _PPSPID} = ssa_gen_server:start(payment_processing_service_split, [], []),
  io:format("started payment processing service successfully~n"),
  {ok, _HBSPID} = ssa_gen_server:start(hotel_booking_service_split, [], []),
  io:format("started hotel booking service successfully~n"),
  {ok, _FBSPID} = ssa_gen_server:start(flight_booking_service_split, [], []),
  io:format("started flight booking service successfully~n"),
  {ok, _CPID} = ssa_gen_server:start(travel_customer_split, [], []),
  io:format("started customer successfully~n"),
  ok.
