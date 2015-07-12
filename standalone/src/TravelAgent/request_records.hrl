-record(customer_booking_request, { name,
                                    origin,
                                    destination,
                                    departure_date,
                                    return_date }).

-record(flight_details, { flight_id,
                          airline,
                          origin_airport,
                          destination_airport,
                          departure_date,
                          arrival_date,
                          departure_time_ust,
                          arrival_time_ust,
                          price
                        } ).

-record(hotel_details,  { hotel_name,
                          check_in_date,
                          check_out_date,
                          price
                        } ).
