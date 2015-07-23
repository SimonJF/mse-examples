-module(travel_customer_split).
-behaviour(ssa_gen_server).
-compile(export_all).
-include("request_records_split.hrl").

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting conversation (customer).~n", []),
  conversation:start_conversation(Monitor, "BookTravel", "Customer"),
  no_state.


fresh_customer_request(CustomerName, Origin, Destination, DepartureDate,
                       ReturnDate) ->
  #customer_booking_request{name=CustomerName, origin=Origin, destination=Destination,
                            departure_date=DepartureDate, return_date=ReturnDate}.

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  if PN == "BookTravel" andalso RN == "Customer" ->
      % TODO: Make this more interactive or something
      CustomerRequest = fresh_customer_request("Simon Fowler", "GLA",
                                               "BUD", "2015-08-22",
                                               "2015-08-29"),
      travel_agent_split:booking_request(ConvKey, CustomerRequest);
     true -> ok
  end,
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(customer, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(customer, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("BookTravel", "Customer", _, _SenderRole, "customerResponse",
                       [OutwardFlightList, ReturnFlightList, HotelDetailList], State, ConvKey) ->
  actor_logger:info(customer, "Response from travel agent:~n Outward Flights: ~p Return Flights: ~p~nHotels: ~p~n",
                    [OutwardFlightList, ReturnFlightList, HotelDetailList]),
  % Check whether we have at least one of each flight & hotel booking,
  % choose the first if we do.
  case {OutwardFlightList, ReturnFlightList, HotelDetailList} of
    % FIXME
    {[Flight1|_], [Flight2|_], [Hotel|_]} ->
      travel_agent_split:booking_proceed(ConvKey, Flight1, Flight2, Hotel);
    _ ->
      % If not, send the cancelBooking message, and kill the session.
      travel_agent_split:booking_cancel(ConvKey),
      conversation:end_conversation(ConvKey, no_suitable_details)
  end,
  {ok, State};
ssactor_handle_message("PerformBooking", "Customer", _, _SenderRole, "ccInfoRequest", _, State, ConvKey) ->
  actor_logger:info(customer, "CC Info request from travel agent ~n", []),
  travel_agent_split:send_cc_info(ConvKey, "123123123", "2019-04-01", 123),
  {ok, State};
ssactor_handle_message("BookTravel", "Customer", _, _, "confirmation", _, State, ConvKey) ->
  conversation:end_conversation(ConvKey, normal),
  {ok, State};
ssactor_handle_message("BookTravel", "Customer", _CID, _SenderRole, Op, Payload, State, _ConvKey) ->
  actor_logger:err(customer, "Unhandled message: (~s,  ~p)", [Op, Payload]),
  {ok, State}.

terminate(_, _) -> ok.

%%%%%%%%
%%% API
%%%%%%%%

booking_response(ConvKey, OutwardFlights, ReturnFlights, Hotels) ->
  conversation:send(ConvKey, ["Customer"], "customerResponse", [],
                    [OutwardFlights, ReturnFlights, Hotels]).

cc_info_request(ConvKey) ->
  conversation:send(ConvKey, ["Customer"], "ccInfoRequest", [], []).

confirmation(ConvKey) ->
  conversation:send(ConvKey, ["Customer"], "confirmation", [], []).
