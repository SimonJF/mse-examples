-module(hotel_booking_service).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(Args, ConvKey) ->
  ok.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  if PN == "BookTravel" andalso RN == "HotelBookingService" ->
      conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]);
     true -> ok
  end,
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(hotel_booking_service, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(hotel_booking_service, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("BookTravel", "HotelBookingService", _CID, SenderRole, "hotelInfoRequest",
                       [{OriginAirport, DestinationAirport, Date, Time}], State, ConvKey) ->
  conversation:send(ConvKey, ["TravelAgent"], "hotelInfoResponse", [], [[]]),
  {ok, no_state};
ssactor_handle_message("BookTravel", "HotelBookingService", _CID, SenderRole, "bookHotel",
                       [{BookingID, Name, CheckinDate, CheckoutDate}], State, ConvKey) ->
  conversation:send(ConvKey, ["TravelAgent"], "hotelBookingConfirmation", [], [[]]),
  {ok, no_state};
ssactor_handle_message("BookTravel", "HotelBookingService", _CID, _SenderRole,
                       Op, Payload, _State, _ConvKey) ->
  actor_logger:err(hotel_booking_service, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, no_state}.

terminate(_, _) -> ok.

