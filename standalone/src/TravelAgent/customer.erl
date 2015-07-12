-module(customer).
-behaviour(ssa_gen_server).
-compile(export_all).
-include("request_records.hrl").

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting conversation (customer).~n", []),
  conversation:start_conversation(Monitor, "", "Customer"),
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
                                               "BUD", "22/8/2015",
                                               "29/8/2015"),
      travel_agent:booking_request(ConvKey, CustomerRequest);
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
                       [FlightDetailList, HotelDetailList], State, ConvKey) ->
  actor_logger:info(customer, "Response from travel agent:~n Flights: ~p~nHotels: ~p~n",
                    [FlightDetailList, HotelDetailList]),
  % Check whether we have at least one of each flight & hotel booking,
  % choose the first if we do.
  case {FlightDetailList, HotelDetailList} of
    {[Flight|_], [Hotel|_]} ->
      travel_agent:booking_proceed(ConvKey, Flight, Hotel);
    _ ->
      % If not, send the cancelBooking message, and kill the session.
      travel_agent:booking_cancel(ConvKey),
      conversation:end_conversation(ConvKey, no_suitable_details)
  end,
  {ok, State};
ssactor_handle_message("BookTravel", "Customer", _, _SenderRole, "ccInfoRequest", _, State, ConvKey) ->
  actor_logger:info(customer, "CC Info request from travel agent ~n", []),
  travel_agent:send_cc_info(ConvKey, "123123123", "01/04/2019", 123),
  {ok, State};
ssactor_handle_message("BookTravel", "Customer", _, _, "confirmation", _, State, ConvKey) ->
  conversation:end_conversation(ConvKey, normal),
  {ok, State};
ssactor_handle_message("BookTravel", "Customer", _CID, _SenderRole, Op, Payload, State, _ConvKey) ->
  actor_logger:err(customer, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, State}.

terminate(_, _) -> ok.

%%%%%%%%
%%% API
%%%%%%%%

booking_response(ConvKey, Flights, Hotels) ->
  conversation:send(ConvKey, ["Customer"], "customerResponse", [],
                    [Flights, Hotels]).

cc_info_request(ConvKey) ->
  conversation:send(ConvKey, ["Customer"], "ccInfoRequest", [], []).

confirmation(ConvKey) ->
  conversation:send(ConvKey, ["Customer"], "confirmation", [], []).
