-module(travel_agent_conf).
-export([config/0]).
% Config file
config() ->
  [{customer, [{"BookTravel", ["Customer"]}]},
   {travel_agent, [{"BookTravel", ["TravelAgent"]}]},
   {hotel_booking_service, [{"BookTravel", ["HotelBookingService"]}]},
   {payment_processing_service, [{"BookTravel", ["PaymentProcessor"]}]},
   {flight_booking_service, [{"BookTravel", ["FlightBookingService"]}]}].
