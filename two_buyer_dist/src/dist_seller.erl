-module(dist_seller).
-behaviour(ssa_gen_server).
-compile(export_all).
-define(PRICE, 40).
-define(DELIVERY_DATE, "Sometime in the future").
-define(SELLER, seller).

% seller:
% Buyer 1 -> seller (title(String))
% seller -> A, B (quote(Int))
% Choice:
%   B -> seller (accept(String))
%     seller -> B(date(String))
%   B -> seller (retry())
%   B -> seller (quit())

ssactor_init(_Args, _Monitor) -> no_state.

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_, _, _, _, State) -> {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "title", [Title], _State, ConvKey) ->
  actor_logger:info(seller, "Received title ~s from ~s", [Title, SenderRole]),
  conversation:send(ConvKey, ["A", "B"], "quote", ["Integer"], [?PRICE]),
  no_state;
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "accept", [Address], _State, ConvKey) ->
  actor_logger:info(seller, "~s accepted quote; received address ~s", [SenderRole, Address]),
  conversation:send(ConvKey, ["B"], "date", ["String"], [?DELIVERY_DATE]),
  no_state;
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "retry", _, _State, _ConvKey) ->
  actor_logger:info(seller, "~s wants to retry", ["TwoBuyers", SenderRole]),
  no_state;
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "quit", _, _State, _ConvKey) ->
  actor_logger:info(seller, "~s wants to quit", ["TwoBuyers", SenderRole]),
  no_state;
ssactor_handle_message("TwoBuyers", "S", _CID, _SenderRole, Op, Payload, _State, _ConvKey) ->
  actor_logger:err(seller, "Unhandled message: (~s, ~w)", [Op, Payload]),
  no_state.


start_link() ->
  ssa_gen_server:start_link({local, ?SELLER}, dist_seller, [], []).
