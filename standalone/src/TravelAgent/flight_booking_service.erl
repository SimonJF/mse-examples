-module(flight_booking_service).
-behaviour(ssa_gen_server).
-compile(export_all).
-include("request_records.hrl").

-record(flight_booking_service_state, { flights,
                                        bookings }).


fresh_state() ->
  OutFlight = #flight_details{flight_id=1,
                              airline="WizzAir",
                              origin_airport="GLA",
                              destination_airport="BUD",
                              departure_date="2015-08-22",
                              arrival_date="2015-08-22",
                              departure_time_ust="09:50",
                              arrival_time_ust="12:45",
                              price="68.00"
                             },
  ReturnFlight= #flight_details{flight_id=2,
                               airline="WizzAir",
                               origin_airport="BUD",
                               destination_airport="GLA",
                               departure_date="2015-08-29",
                               arrival_date="2015-08-29",
                               departure_time_ust="06:20",
                               arrival_time_ust="09:20",
                               price="68.00"
                             },
  FlightList = [OutFlight, ReturnFlight],
  #flight_booking_service_state{flights=FlightList,
                                bookings=orddict:new()}.

add_booking(OutwardID, ReturnID, Name, State) ->
  Bookings = State#flight_booking_service_state.bookings,
  Bookings1 = orddict:append(OutwardID, Name, Bookings),
  Bookings2 =
    if ReturnID =/= -1 ->
      orddict:append(ReturnID, Name, Bookings);
       ReturnID == -1 ->
         Bookings1
    end,
  State#flight_booking_service_state{bookings=Bookings2}.

find_flights(OriginAirport, DepartureDate, State) ->
  find_flights_inner(State#flight_booking_service_state.flights,
                     OriginAirport, DepartureDate).

find_flights_inner([], _OriginAirport, _DepartureDate) -> [];
find_flights_inner([Flight|Flights], OriginAirport, DepartureDate) ->
  OA = Flight#flight_details.origin_airport,
  DD = Flight#flight_details.departure_date,
  if OA == OriginAirport andalso DD == DepartureDate ->
       [Flight|find_flights(Flights, OriginAirport, DepartureDate)];
     true ->
       find_flights(Flights, OriginAirport, DepartureDate)
  end.

ssactor_init(_Args, _ConvKey) ->
  fresh_state().


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(flight_booking_service, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(flight_booking_service, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("BookTravel", "FlightBookingService", _CID, _SenderRole,
                       "flightInfoRequest", [CustomerRequest], State, ConvKey) ->
  DepartureDate = CustomerRequest#customer_booking_request.departure_date,
  ReturnDate = CustomerRequest#customer_booking_request.return_date,
  Origin = CustomerRequest#customer_booking_request.origin,
  Destination = CustomerRequest#customer_booking_request.destination,
  OutwardFlights = find_flights(Origin, DepartureDate, State),
  ReturnFlights = find_flights(Destination, ReturnDate, State),
  travel_agent:send_flight_info(ConvKey, [OutwardFlights, ReturnFlights]),
  {ok, State};
ssactor_handle_message("BookTravel", "FlightBookingService", _CID, _SenderRole, "bookFlight",
                       [Name, OutwardFlightID, ReturnFlightID], State, ConvKey) ->
  State1 = add_booking(OutwardFlightID, ReturnFlightID, Name, State),
  travel_agent:flight_confirmation(ConvKey),
  {ok, State1};
ssactor_handle_message("BookTravel", "FlightBookingService", _CID, _SenderRole,
                       Op, Payload, State, _ConvKey) ->
  actor_logger:err(flight_booking_service, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, State}.

terminate(_, _) -> ok.

%%%%%%%%%%
%%%%% API
%%%%%%%%%%

get_flights(ConvKey, CustomerRequest) ->
  conversation:send(ConvKey, ["FlightBookingService"], "flightInfoRequest", [],
                    [CustomerRequest]).

book_flight(ConvKey, CustomerName, OutwardFlightID, ReturnFlightID) ->
  conversation:send(ConvKey, ["FlightBookingService"], "bookFlight", [],
                    [CustomerName, OutwardFlightID, ReturnFlightID]).
