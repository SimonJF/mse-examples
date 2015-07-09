-record(customer_booking_request, { name,
                                    origin,
                                    destination,
                                    departure_date,
                                    return_date }).

-record(customer_booking_response, { flights,
                                     hotels } ).

-record(flight_details, { flight_id,
                          airline,
                          departure_date_time,
                          arrival_date_time } ).
