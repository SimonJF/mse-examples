-module(hotel_booking_service).
-behaviour(ssa_gen_server).
-compile(export_all).
-include("request_records.hrl").

-record(hotel_booking_service_state, { hotels,
                                       bookings }).

-record(hotel_booking, { name,
                         check_in_date,
                         check_out_date }).


fresh_state() ->
  Hotels = [{"BUD", ["Grand Budapest"]}],
  #hotel_booking_service_state{hotels=orddict:from_list(Hotels),
                               bookings=orddict:new()}.

create_booking(Name, CheckinDate, CheckoutDate) ->
  #hotel_booking{name=Name,
                 check_in_date=CheckinDate,
                 check_out_date=CheckoutDate}.

add_booking(Name, HotelName, CheckinDate, CheckoutDate, State) ->
  Bookings = State#hotel_booking_service_state.bookings,
  NewBookingEntry = create_booking(Name, CheckinDate, CheckoutDate),
  NewBookings = orddict:append(HotelName, NewBookingEntry, Bookings),
  State#hotel_booking_service_state{bookings=NewBookings}.

ssactor_init(_Args, _MonitorPID) ->
  fresh_state().

find_hotels(City, State) ->
  case orddict:find(City, State#hotel_booking_service_state.hotels) of
    {ok, HotelList} -> HotelList;
    _ -> []
  end.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_PN, _RN, _CID, _ConvKey, State) ->
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(hotel_booking_service, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(hotel_booking_service, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("BookTravel", "HotelBookingService", _CID, _SenderRole,
                       "hotelInfoRequest", [CustomerBookingRequest], State, ConvKey) ->
  City = CustomerBookingRequest#customer_booking_request.destination,
  Hotels = find_hotels(City, State),
  travel_agent:send_hotel_info(ConvKey, Hotels),
  {ok, State};
ssactor_handle_message("BookTravel", "HotelBookingService", _CID, _SenderRole, "bookHotel",
                       [Name, HotelName, CheckinDate, CheckoutDate], State, ConvKey) ->
  NewState = add_booking(Name, HotelName, CheckinDate, CheckoutDate, State),
  travel_agent:hotel_confirmation(ConvKey),
  {ok, NewState};
ssactor_handle_message("BookTravel", "HotelBookingService", _CID, _SenderRole,
                       Op, Payload, _State, _ConvKey) ->
  actor_logger:err(hotel_booking_service, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, no_state}.

terminate(_, _) -> ok.


%%%%%%%%
%%% API
%%%%%%%%


get_hotels(ConvKey, CustomerRequest) ->
  conversation:send(ConvKey, ["FlightBookingService"], "hotelInfoRequest", [],
                    [CustomerRequest]).

book_hotel(ConvKey, CustomerName, HotelName, CheckInDate, CheckOutDate) ->
  conversation:send(ConvKey, ["FlightBookingService"], "bookFlight", [],
                    [CustomerName, HotelName, CheckInDate, CheckOutDate]).
