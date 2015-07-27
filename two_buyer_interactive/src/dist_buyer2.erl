-module(dist_buyer2).
-behaviour(buyer2).
-compile(export_all).
-define(PRICE_THRESHOLD, 50).
-define(BUYER2, dist_buyer2).
-define(CONV_REG_ATOM, buyer_conv).

% Buyer 2:
%   Server -> Buyer 2 (quote(Int))
%   Buyer 1 -> Buyer 2 (share(Int))
%   Choice:
%     Buyer 2 -> Buyer 1, Server (accept(String))
%       Server -> Buyer 2 (date(String))
%     Buyer 2 -> Buyer 1, Server (retry())
%     Buyer 2 -> Buyer 1, Server (quit())

ssactor_init(_Args, MonitorPID) -> MonitorPID.

ssactor_join(_, _, _, State) -> {accept, State}.
ssactor_conversation_established(_, _, _, ConvKey, State) ->
  conversation:register_conversation(?CONV_REG_ATOM, ConvKey),
  {ok, State}.
ssactor_conversation_error(_, _, _, State) -> {ok, State}.

ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "quote", [QuoteInt], State, _Monitor) ->
  actor_logger:info(buyer2, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "share", [Share], State, Monitor) ->
  actor_logger:info(buyer2, "Received share quote (~p) from ~s", [Share, SenderRole]),
  if Share >= ?PRICE_THRESHOLD ->
       % Nah, we aint paying that
       actor_logger:info(buyer2, "Rejected share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(Monitor, ["A", "S"], "quit", [], []);
     Share < ?PRICE_THRESHOLD ->
       % We can afford it: accept, send address to buyer2 and server,
       % and retrieve the delivery date from the server
       actor_logger:info(buyer2, "Accepted share quote (threshold ~p)", [?PRICE_THRESHOLD])
  end,
  {ok, State};
ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "date", [DeliveryDate], State, _Monitor) ->
  actor_logger:info(buyer2, "Received delivery date of ~s from ~s", [DeliveryDate, SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "B", _CID, _SenderRole, Op, Payload, State, _Monitor) ->
  actor_logger:err(buyer2, "Unhandled message: (~s, ~w)", [Op, Payload]),
  {ok, State}.

% Become
ssactor_become(_, _, send_title, _Args, ConvKey, State) ->
  conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]),
  {ok, State};
ssactor_become(_, _, send_accept, _Args, ConvKey, State) ->
  conversation:send(ConvKey, ["A", "S"], "accept",
                         ["String"], ["Informatics Forum"]),
  {ok, State}.

handle_call(_, _, State) -> {noreply, State}.

handle_cast(send_title, MonitorPID) ->
  io:format("Handling send_title cast.~n", []),
  conversation:become(MonitorPID, ?CONV_REG_ATOM, "B", send_title, []),
  {noreply, MonitorPID};
handle_cast(send_accept, MonitorPID) ->
  io:format("Handling send_title cast.~n", []),
  conversation:become(MonitorPID, ?CONV_REG_ATOM, "B", send_accept, []),
  {noreply, MonitorPID}.

terminate(_, _) -> ok.

%%%%%%%%%%
%%% API
%%%%%%%%%%

start_link() ->
  ssa_gen_server:start_link({local, ?BUYER2}, dist_buyer2, [], []).

send_accept() ->
  ssa_gen_server:cast(?BUYER2, send_accept).

send_title() ->
  ssa_gen_server:cast(?BUYER2, send_title).

