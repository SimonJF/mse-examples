-module(dist_seller).
-behaviour(ssa_gen_server).
-compile(export_all).
-define(PRICE, 40).
-define(DELIVERY_DATE, "Sometime in the future").
-define(SELLER, seller).
-define(CONV_REG_ATOM, buyer_conv).

% seller:
% Buyer 1 -> seller (title(String))
% seller -> A, B (quote(Int))
% Choice:
%   B -> seller (accept(String))
%     seller -> B(date(String))
%   B -> seller (retry())
%   B -> seller (quit())

ssactor_init(_Args, MonitorPID) -> MonitorPID.

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_, _, _, ConvKey, State) ->
  conversation:register_conversation(?CONV_REG_ATOM, ConvKey),
  {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "title", [Title], State, _ConvKey) ->
  actor_logger:info(seller, "Received title ~s from ~s", [Title, SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "accept", [Address], State, _ConvKey) ->
  actor_logger:info(seller, "~s accepted quote; received address ~s", [SenderRole, Address]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "retry", _, State, _ConvKey) ->
  actor_logger:info(seller, "~s wants to retry", ["TwoBuyers", SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "S", _CID, SenderRole, "quit", _, State, _ConvKey) ->
  actor_logger:info(seller, "~s wants to quit", ["TwoBuyers", SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "S", _CID, _SenderRole, Op, Payload, State, _ConvKey) ->
  actor_logger:err(seller, "Unhandled message: (~s, ~w)", [Op, Payload]),
  {ok, State}.


handle_call(_, _, State) -> {noreply, State}.

handle_cast(send_quote, MonitorPID) ->
  io:format("Handling send_quote cast.~n", []),
  conversation:become(MonitorPID, ?CONV_REG_ATOM, "S", send_quote, []),
  {noreply, MonitorPID};
handle_cast(send_date, MonitorPID) ->
  io:format("Handling send_quote cast.~n", []),
  conversation:become(MonitorPID, ?CONV_REG_ATOM, "S", send_date, []),
  {noreply, MonitorPID}.


terminate(_, _) -> ok.

% Become
ssactor_become(_, _, send_quote, _Args, ConvKey, State) ->
  conversation:send(ConvKey, ["A", "B"], "quote", ["Integer"], [?PRICE]),
  {ok, State};
ssactor_become(_, _, send_date, _Args, ConvKey, State) ->
  conversation:send(ConvKey, ["B"], "date", ["String"], [?DELIVERY_DATE]),
  {ok, State}.

ssactor_conversation_ended(_CID, Reason, UserState) ->
  actor_logger:info(seller, "Conv ended for reason ~p", [Reason]),
  {ok, UserState}.


%%%%%%%%%%
%%% API
%%%%%%%%%%

send_quote() ->
  ssa_gen_server:cast(?SELLER, send_quote).

send_date() ->
  ssa_gen_server:cast(?SELLER, send_date).

start_link() ->
  ssa_gen_server:start_link({local, ?SELLER}, dist_seller, [], []).

start_link(NameAtom) ->
  ssa_gen_server:start_link({local, NameAtom}, dist_seller, [], []).

kill() ->
  exit(?SELLER, kill).

kill(NameAtom) ->
  exit(NameAtom, kill).


