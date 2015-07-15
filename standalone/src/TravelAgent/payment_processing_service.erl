-module(payment_processing_service).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(Args, ConvKey) ->
  ok.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(payment_processing_service, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(payment_processing_service, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("BookTravel", "PaymentProcessor", _CID, SenderRole, "processPayment",
                       [_CCNumber, _ExpiryDate, _CVC, _Money], State, ConvKey) ->
  travel_agent:payment_confirmation(ConvKey),
  {ok, State};
ssactor_handle_message(_, _, _CID, _SenderRole,
                       "cancelBooking", _Payload, State, _ConvKey) ->
  {ok, State};
ssactor_handle_message("BookTravel", "PaymentProcessor", _CID, _SenderRole,
                       Op, Payload, State, _ConvKey) ->
  actor_logger:err(payment_processing_service, "Unhandled message: (~s,  ~w)",
                   [Op, Payload]),
  {ok, State}.

terminate(_, _) -> ok.

%%%%%
%%% API
%%%%%

process_payment(ConvKey, CCNumber, ExpiryDate, CVC, Money) ->
  conversation:send(ConvKey, "processPayment", [],
                    [CCNumber, ExpiryDate, CVC, Money]).

