-module(payment_processing_service).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(Args, ConvKey) ->
  ok.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  if PN == "BookTravel" andalso RN == "PaymentProcessor" ->
      conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]);
     true -> ok
  end,
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(payment_processing_service, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(payment_processing_service, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("BookTravel", "PaymentProcessor", _CID, SenderRole, "processPayment",
                       [CCNumber, ExpiryDate, CVC, Money], State, ConvKey) ->
  conversation:send(ConvKey, ["TravelAgent"], "flightInfoResponse", [], [[]]),
  {ok, no_state};
ssactor_handle_message("BookTravel", "PaymentProcessor", _CID, _SenderRole,
                       Op, Payload, _State, _ConvKey) ->
  actor_logger:err(payment_processing_service, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, no_state}.

terminate(_, _) -> ok.

