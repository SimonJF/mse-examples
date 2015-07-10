-module(travel_agent).
-behaviour(ssa_gen_server).
-compile(export_all).

-record(travel_agent_state, { customer_info }).

-record(booking_info, { customer_request,
                        returned_flights,
                        returned_hotels,
                        flight_booked=false,
                        hotel_booked=false,
                      }).

fresh_state() ->
  #travel_agent_state{customer_info=orddict:new()}.

fresh_booking_info(Request) ->
  #booking_info{customer_request=Request,
                returned_flights=undefined,
                returned_hotels=undefined}.

add_state_entry(CID, Request, State) ->
  CustomerInfo = State#travel_agent_state.customer_info,
  NewCustomerInfo = orddict:store(CID, fresh_booking_info(),
                                  CustomerInfo).

ssactor_init(Args, _Monitor) ->
  fresh_state().

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(travel_agent, "Could not establish conversation: ~p~n", [Error]),

  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(travel_agent, "Conversation ~p ended.~n", [CID]),
  {ok, State}.


get_received_info(CID, State) ->
  BookingInfo = orddict:fetch(CID, State#travel_agent_state.customer_info),
  {BookingInfo#booking_info.returned_flights,
   BookingInfo#booking_info.returned_hotels}.


ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "customerRequest",
                       [CustomerRequest], State, ConvKey) ->
  % Request details from both flight and hotel providers
  flight_booking_service:get_flights(ConvKey, CustomerRequest),
  hotel_booking_service:get_hotels(ConvKey, CustomerRequest),
  NewState = add_state_entry(CID, CustomerRequest, State),
  {ReturnedFlights, ReturnedHotels} = get_received_info(CID, NewState),
  if ReturnedFlights =/= undefined andalso ReturnedHotels =/= undefined ->
       % If both have been received, we can send the flights and
       % hotel details back to the user
       customer:booking_response(ConvKey, ReturnedFlights, ReturnedHotels);
     true -> ok
  end,
  {ok, NewState};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, "flightInfoResponse",
                       [FlightDetailList], State, ConvKey) ->

  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, "hotelInfoResponse",
                       [HotelDetailList], State, ConvKey) ->
  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, "cancelBooking", [],
                       State, _ConvKey) ->
  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, "proceedWithBooking", [],
                       State, _ConvKey) ->
  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, "ccInfoResponse", [],
                       State, _ConvKey) ->
  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, "flightBookingConfirmation", [],
                       State, _ConvKey) ->
  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, "hotelBookingConfirmation", [],
                       State, _ConvKey) ->
  {ok, State};

ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, Op, Payload, State, _ConvKey) ->
  actor_logger:err(travel_agent, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, State}.

terminate(_, _) -> ok.


%%%%%%
%%% API
%%%%%%

booking_request(ConvKey, CustomerRequest) ->
  conversation:send(ConvKey, ["TravelAgent"], "customerRequest", [],
                    [CustomerRequest]).

booking_proceed(ConvKey, FlightID, HotelID) ->
  conversation:send(ConvKey, ["TravelAgent"], "proceedWithBooking", [], [FlightID, HotelID]).

booking_cancel(ConvKey) ->
  conversation:send(ConvKey, ["TravelAgent"], "cancelBooking", [], []).

send_cc_info(ConvKey, CCNum, ExpiryDate, CVC) ->
  conversation:send(ConvKey, ["TravelAgent"], "ccInfoResponse", [],
                    [CCNum, ExpiryDate, CVC]).

send_flight_info(ConvKey, FlightInfo) ->
  conversation:send(ConvKey, ["TravelAgent"], "flightInfoResponse", [],
                    [FlightInfo]).

send_hotel_info(ConvKey, HotelInfo) ->
  conversation:send(ConvKey, ["TravelAgent"], "hotelInfoResponse", [],
                    [HotelInfo]).

flight_confirmation(ConvKey) ->
  conversation:send(ConvKey, ["TravelAgent"], "flightBookingConfirmation", [],
                    []).

hotel_confirmation(ConvKey) ->
  conversation:send(ConvKey, ["TravelAgent"], "hotelBookingConfirmation", [],
                    []).


payment_confirmation(ConvKey) ->
  conversation:send(ConvKey, ["TravelAgent"], "paymentConfirmation", [], []).
