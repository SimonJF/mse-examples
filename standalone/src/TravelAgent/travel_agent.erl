-module(travel_agent).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(Args, ConvKey) ->
  ok.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  if PN == "BookTravel" andalso RN == "TravelAgent" ->
      conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]);
     true -> ok
  end,
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(travel_agent, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(travel_agent, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("BookTravel", "TravelAgent", _CID, SenderRole, "customerRequest",
                       [QuoteInt], State, ConvKey) ->
  conversation:send(ConvKey, ["B"], "share", ["Integer"], [QuoteInt div 2]),
  {ok, no_state};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, SenderRole, "flightInfoResponse",
                       [FlightDetailList], State, ConvKey) ->
  {ok, no_state};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, SenderRole, "hotelInfoResponse",
                       [HotelDetailList], State, ConvKey) ->
  {ok, no_state};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, SenderRole, "cancelBooking", [],
                       _State, _ConvKey) ->
  {ok, no_state};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, SenderRole, "proceedWithBooking", [],
                       _State, _ConvKey) ->
  {ok, no_state};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, SenderRole, "ccInfoResponse", [],
                       _State, _ConvKey) ->
  {ok, no_state};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, SenderRole, "flightBookingConfirmation", [],
                       _State, _ConvKey) ->
  {ok, no_state};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, SenderRole, "hotelBookingConfirmation", [],
                       _State, _ConvKey) ->
  {ok, no_state};

ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, Op, Payload, _State, _ConvKey) ->
  actor_logger:err(travel_agent, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, no_state}.

terminate(_, _) -> ok.
