-module(reflexive_seller).
-behaviour(ssa_gen_server).
-compile(export_all).
-define(PRICE, 40).
-define(DELIVERY_DATE, "Sometime in the future").

% reflexive_seller:
% Buyer 1 -> reflexive_seller (title(String))
% reflexive_seller -> A, B (quote(Int))
% Choice:
%   B -> reflexive_seller (accept(String))
%     reflexive_seller -> B(date(String))
%   B -> reflexive_seller (retry())
%   B -> reflexive_seller (quit())

ssactor_init(_Args, _Monitor) -> {ok, no_state}.

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_, _, _, _, State) -> {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(reflexive_seller, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "title", [Title], _State, ConvKey) ->
  actor_logger:info(reflexive_seller, "Received title ~s from ~s", [Title, SenderRole]),
  conversation:send(ConvKey, ["A", "B"], "quote", ["Integer"], [?PRICE]),
  {ok, no_state};
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "accept", [Address], _State, ConvKey) ->
  actor_logger:info(reflexive_seller, "~s accepted quote; received address ~s", [SenderRole, Address]),
  conversation:send(ConvKey, ["B"], "date", ["String"], [?DELIVERY_DATE]),
  {ok, no_state};
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "retry", _, _State, _ConvKey) ->
  actor_logger:info(reflexive_seller, "~s wants to retry", ["TwoBuyers", SenderRole]),
  {ok, no_state};
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "quit", _, _State, _ConvKey) ->
  actor_logger:info(reflexive_seller, "~s wants to quit", ["TwoBuyers", SenderRole]),
  {ok, no_state};
ssactor_handle_message("TwoBuyers", "S", _CID, _SenderRole, Op, Payload, _State, _ConvKey) ->
  actor_logger:err(reflexive_seller, "Unhandled message: (~s, ~w)", [Op, Payload]),
  {ok, no_state}.

