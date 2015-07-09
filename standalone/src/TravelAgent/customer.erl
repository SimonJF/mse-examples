-module(travel_agent).
-behaviour(ssa_gen_server).
-compile(export_all).
-include("request_records.hrl").

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting conversation (customer).~n", []),
  conversation:start_conversation(Monitor, "", "Customer"),
  no_state.


fresh_customer_request(CustomerName, Origin, Destination, DepartureDate,
                       ReturnDate) ->
  #customer_request{name=CustomerName, origin=Origin, destination=Destination,
                    departure_date=DepartureDate, return_date=ReturnDate}.

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  if PN == "BookTravel" andalso RN == "Customer" ->
      % TODO: Make this more interactive or something
      CustomerRequest = fresh_customer_request("Simon Fowler", "Edinburgh",
                                               "Budapest", "22/8/2015",
                                               "29/8/15"),
      conversation:send(ConvKey, ["TravelAgent"], "customerRequest", [],
                        [CustomerRequest]);
     true -> ok
  end,
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(buyer1, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(buyer1, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("BookTravel", "Customer", _, SenderRole, "customerResponse",
                       [CustomerResponse], _State, ConvKey) ->
  actor_logger:info(buyer1, "Response from travel agent:", [QuoteInt, SenderRole]),
  conversation:send(ConvKey, ["B"], "share", ["Integer"], [QuoteInt div 2]),
  {ok, no_state};
ssactor_handle_message("BookTravel", "Customer", _, SenderRole, "accept", [Address], _State, _ConvKey) ->
  actor_logger:info(buyer1, "~s accepted quote; received address (~p)", [SenderRole, Address]),
  {ok, no_state};
ssactor_handle_message("BookTravel", "Customer", _, SenderRole, "retry", _, _State, _ConvKey) ->
  actor_logger:info(buyer1, "~s wants to retry", [SenderRole]),
  {ok, no_state};
ssactor_handle_message("BookTravel", "Customer", _, SenderRole, "quit", _, _State, _ConvKey) ->
  actor_logger:info(buyer1, "~s wants to quit", [SenderRole]),
  {ok, no_state};
ssactor_handle_message("BookTravel", "Customer", _CID, _SenderRole, Op, Payload, _State, _ConvKey) ->
  actor_logger:err(buyer1, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, no_state}.

terminate(_, _) -> ok.
