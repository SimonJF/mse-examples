-module(travel_agent_split_conf).
-export([config/0]).
% Config file
config() ->
  [{travel_customer_split, [{"BookTravel", ["Customer"]},
                            {"PerformBooking", ["Customer"]}
                           ]},
   {travel_agent_split, [{"BookTravel", ["TravelAgent"]},
                         {"PerformBooking", ["TravelAgent"]},
                         {"PerformPayment", ["TravelAgent"]},
                         {"CancelBookings", ["TravelAgent"]}
                        ]},
   {hotel_booking_service_split, [{"BookTravel", ["HotelBookingService"]},
                                  {"PerformBooking", ["HotelBookingService"]},
                                  {"CancelBookings", ["HotelBookingService"]}
                                 ]},
   {payment_processing_service_split, [{"BookTravel", ["PaymentProcessor"]},
                                       {"PerformPayment", ["PaymentProcessor"]}
                                      ]},
   {flight_booking_service_split, [{"BookTravel", ["FlightBookingService"]},
                                  {"PerformBooking", ["FlightBookingService"]},
                                  {"CancelBookings", ["FlightBookingService"]}
                                 ]}].
