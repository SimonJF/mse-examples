-module(travel_agent).
-behaviour(ssa_gen_server).
-compile(export_all).
-include("request_records.hrl").

-record(travel_agent_state, { customer_info }).

-record(booking_info, { customer_request,
                        returned_outward_flights,
                        returned_return_flights,
                        returned_hotels,
                        chosen_flights,
                        chosen_hotel,
                        flights_booked=false,
                        hotel_booked=false,
                        cc_number,
                        cc_expiry_date,
                        cc_cvc
                      }).

fresh_state() ->
  #travel_agent_state{customer_info=orddict:new()}.

fresh_booking_info(Request) ->
  #booking_info{customer_request=Request,
                returned_outward_flights=undefined,
                returned_return_flights=undefined,
                returned_hotels=undefined}.

add_state_entry(CID, Request, State) ->
  CustomerInfo = State#travel_agent_state.customer_info,
  NewCustomerInfo = orddict:store(CID, fresh_booking_info(Request),
                                  CustomerInfo),
  State#travel_agent_state{customer_info=NewCustomerInfo}.

record_selections(CID, ChosenFlights, ChosenHotel, State) ->
  CustomerInfo = State#travel_agent_state.customer_info,
  BookingInfo = orddict:fetch(CID, CustomerInfo),
  NewBookingInfo = BookingInfo#booking_info{chosen_flights=ChosenFlights,
                                            chosen_hotel=ChosenHotel},
  NewCustomerInfo = orddict:store(CID, NewBookingInfo, CustomerInfo),
  State#travel_agent_state{customer_info=NewCustomerInfo}.

record_cc_info(CID, CCNumber, ExpiryDate, CVC, State) ->
  CustomerInfo = State#travel_agent_state.customer_info,
  BookingInfo = orddict:fetch(CID, CustomerInfo),
  NewBookingInfo = BookingInfo#booking_info{cc_number=CCNumber,
                                            cc_expiry_date=ExpiryDate,
                                            cc_cvc=CVC
                                           },
  NewCustomerInfo = orddict:store(CID, NewBookingInfo, CustomerInfo),
  State#travel_agent_state{customer_info=NewCustomerInfo}.

update_booking_info(CID, NewBookingInfo, State) ->
  CustomerInfo = State#travel_agent_state.customer_info,
  NewCustomerInfo = orddict:store(CID, NewBookingInfo, CustomerInfo),
  State#travel_agent_state{customer_info=NewCustomerInfo}.

calculate_price(CID, BookingInfo) ->
  [OutwardFlight, ReturnFlight] = BookingInfo#booking_info.chosen_flights,
  OutwardFlightPrice = OutwardFlight#flight_details.price,
  ReturnFlightPrice = ReturnFlight#flight_details.price,
  Hotel = BookingInfo#booking_info.chosen_hotel,
  HotelPrice = Hotel#hotel_details.price,
  OutwardFlightPrice + ReturnFlightPrice + HotelPrice.

check_booked(CID, ConvKey, State) ->
  BookingInfo = get_booking_info(CID, State),
  HotelBooked = BookingInfo#booking_info.hotel_booked,
  FlightsBooked = BookingInfo#booking_info.flights_booked,
  if HotelBooked andalso FlightsBooked ->
       CCNumber = BookingInfo#booking_info.cc_number,
       ExpiryDate = BookingInfo#booking_info.cc_expiry_date,
       CVC = BookingInfo#booking_info.cc_cvc,
       Price = calculate_price(CID, BookingInfo),
       payment_processing_service:process_payment(ConvKey, CCNumber,
                                                  ExpiryDate, CVC, Price);
     true -> ok
  end,
  ok.

record_hotel_success(CID, ConvKey, State) ->
  BookingInfo = get_booking_info(CID, State),
  NewBookingInfo = BookingInfo#booking_info{hotel_booked=true},
  NewState = update_booking_info(CID, NewBookingInfo, State),
  check_booked(CID, ConvKey, NewState),
  NewState.

record_flight_success(CID, ConvKey, State) ->
  BookingInfo = get_booking_info(CID, State),
  NewBookingInfo = BookingInfo#booking_info{flights_booked=true},
  NewState = update_booking_info(CID, NewBookingInfo, State),
  check_booked(CID, ConvKey, NewState),
  NewState.


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


check_received_info(ConvKey, CID, State) ->
  BookingInfo = orddict:fetch(CID, State#travel_agent_state.customer_info),
  ReturnedOutwardFlights = BookingInfo#booking_info.returned_outward_flights,
  ReturnedReturnFlights = BookingInfo#booking_info.returned_return_flights,
  ReturnedHotels = BookingInfo#booking_info.returned_hotels,
  if ReturnedOutwardFlights =/= undefined andalso ReturnedReturnFlights =/= undefined
     andalso ReturnedHotels =/= undefined ->
       % If both have been received, we can send the flights and
       % hotel details back to the user
       travel_customer:booking_response(ConvKey, ReturnedOutwardFlights,
                                        ReturnedReturnFlights, ReturnedHotels);
     true -> ok
  end.

add_hotel_response(CID, HotelResponse, State) ->
  CustomerInfo = State#travel_agent_state.customer_info,
  BookingInfo = orddict:fetch(CID, CustomerInfo),
  NewBookingInfo = BookingInfo#booking_info{returned_hotels=HotelResponse},
  NewCustomerInfo = orddict:store(CID, NewBookingInfo, CustomerInfo),
  State#travel_agent_state{customer_info=NewCustomerInfo}.


add_flight_response(CID, OutwardFlightResponse, ReturnFlightResponse, State) ->
  CustomerInfo = State#travel_agent_state.customer_info,
  BookingInfo = orddict:fetch(CID, CustomerInfo),
  NewBookingInfo = BookingInfo#booking_info{returned_outward_flights=OutwardFlightResponse,
                                            returned_return_flights=ReturnFlightResponse
                                           },
  NewCustomerInfo = orddict:store(CID, NewBookingInfo, CustomerInfo),
  State#travel_agent_state{customer_info=NewCustomerInfo}.

get_booking_info(CID, State) ->
  BookingInfo = orddict:fetch(CID, State#travel_agent_state.customer_info).

book_flights_and_hotel(CID, ConvKey, State) ->
  BookingInfo = get_booking_info(CID, State),
  [OutwardFlight, ReturnFlight] = BookingInfo#booking_info.chosen_flights,
  Hotel = BookingInfo#booking_info.chosen_hotel,
  CR = BookingInfo#booking_info.customer_request,
  Name = CR#customer_booking_request.name,
  error_logger:info_msg("Hotel: ~p~n", [Hotel]),
  HotelName = Hotel#hotel_details.hotel_name,
  CheckInDate = Hotel#hotel_details.check_in_date,
  CheckOutDate = Hotel#hotel_details.check_out_date,
  flight_booking_service:book_flight(ConvKey, Name, OutwardFlight, ReturnFlight),
  hotel_booking_service:book_hotel(ConvKey, Name, HotelName, CheckInDate, CheckOutDate).

ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "customerRequest",
                       [CustomerRequest], State, ConvKey) ->
  % Request details from both flight and hotel providers
  flight_booking_service:get_flights(ConvKey, CustomerRequest),
  hotel_booking_service:get_hotels(ConvKey, CustomerRequest),
  NewState = add_state_entry(CID, CustomerRequest, State),
  {ok, NewState};
ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "flightInfoResponse",
                       [OutwardFlights, ReturnFlights], State, ConvKey) ->
  NewState = add_flight_response(CID, OutwardFlights, ReturnFlights, State),
  check_received_info(ConvKey, CID, NewState),
  {ok, NewState};
ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "hotelInfoResponse",
                       [HotelDetailList], State, ConvKey) ->
  NewState = add_hotel_response(CID, HotelDetailList, State),
  check_received_info(ConvKey, CID, NewState),
  {ok, NewState};
ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "cancelBooking", [],
                       State, _ConvKey) ->
  actor_logger:info("Booking cancelled for ~p~n", [CID]),
  {ok, State};

ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "proceedWithBooking",
                       [ChosenOutwardFlight, ChosenReturnFlight, ChosenHotel], State, ConvKey) ->
  NewState = record_selections(CID, [ChosenOutwardFlight, ChosenReturnFlight], ChosenHotel, State),
  travel_customer:cc_info_request(ConvKey),
  {ok, NewState};
ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "ccInfoResponse",
                      [CCNum, ExpiryDate, CVC], State, ConvKey) ->
  NewState = record_cc_info(CID, CCNum, ExpiryDate, CVC, State),
  book_flights_and_hotel(CID, ConvKey, NewState),
  {ok, NewState};
ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "flightBookingConfirmation", [],
                       State, ConvKey) ->
  record_flight_success(CID, ConvKey, State),
  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", CID, _SenderRole, "hotelBookingConfirmation", [],
                       State, ConvKey) ->
  record_hotel_success(CID, ConvKey, State),
  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, "paymentConfirmation", [],
                       State, ConvKey) ->
  travel_customer:confirmation(ConvKey),
  {ok, State};
ssactor_handle_message("BookTravel", "TravelAgent", _CID, _SenderRole, Op, Payload, State, _ConvKey) ->
  actor_logger:err(travel_agent, "Unhandled message: (~s,  ~p)", [Op, Payload]),
  {ok, State}.

terminate(_, _) -> ok.


%%%%%%
%%% API
%%%%%%

booking_request(ConvKey, CustomerRequest) ->
  conversation:send(ConvKey, ["TravelAgent"], "customerRequest", [],
                    [CustomerRequest]).

booking_proceed(ConvKey, OutwardFlight, ReturnFlight, HotelID) ->
  conversation:send(ConvKey, ["TravelAgent"], "proceedWithBooking", [], [OutwardFlight, ReturnFlight, HotelID]).

booking_cancel(ConvKey) ->
  conversation:send(ConvKey, ["TravelAgent"], "cancelBooking", [], []).

send_cc_info(ConvKey, CCNum, ExpiryDate, CVC) ->
  conversation:send(ConvKey, ["TravelAgent"], "ccInfoResponse", [],
                    [CCNum, ExpiryDate, CVC]).

send_flight_info(ConvKey, OutwardFlightInfo, ReturnFlightInfo) ->
  conversation:send(ConvKey, ["TravelAgent"], "flightInfoResponse", [],
                    [OutwardFlightInfo, ReturnFlightInfo]).

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
