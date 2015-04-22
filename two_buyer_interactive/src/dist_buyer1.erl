-module(dist_buyer1).
-behaviour(ssa_gen_server).
-compile(export_all).
-define(BUYER1, buyer1).
-define(CONV_REG_ATOM, buyer_conv).

% Buyer 1:
%   Buyer 1 -> Server (title(String))
%   Server -> Buyer 1 (quote(Int))
%   Buyer 1 -> Buyer 2 (share(Int))
%   Choice:
%     Buyer2 -> Buyer 1 (accept(String))
%     Buyer2 -> Buyer 1 (retry())
%     Buyer2 -> Buyer 1 (quit())

ssactor_init(_Args, ProxyPID) ->
  % Start the conversation
  io:format("Starting conversation in buyer1.~n", []),
  conversation:start_conversation(ProxyPID, "TwoBuyers", "A"),
  ProxyPID.

ssactor_join(_, _, _, State) ->
  error_logger:info_msg("Buyer1 joined conversation.~n"),
  {accept, State}.

ssactor_conversation_established(_PN, _RN, _CID, ConvKey, State) ->
  conversation:register_conversation(?CONV_REG_ATOM, ConvKey),
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:error(buyer1, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "quote", [QuoteInt], State, ConvKey) ->
  actor_logger:info(buyer1, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  conversation:send(ConvKey, ["B"], "share", ["Integer"], [QuoteInt div 2]),
  State;
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "accept", [Address], State, _ConvKey) ->
  actor_logger:info(buyer1, "~s accepted quote; received address (~p)", [SenderRole, Address]),
  State;
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "retry", _, State, _ConvKey) ->
  actor_logger:info(buyer1, "~s wants to retry", [SenderRole]),
  State;
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "quit", _, State, _ConvKey) ->
  actor_logger:info(buyer1, "~s wants to quit", [SenderRole]),
  State;
ssactor_handle_message("TwoBuyers", "A", _CID, _SenderRole, Op, Payload, State, _ConvKey) ->
  actor_logger:err(buyer1, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  State.

% Become
ssactor_become(_, _, send_title, _Args, ConvKey, State) ->
  conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]),
  State.

terminate(_, _) -> ok.


%%%%%%%% API Functions
%%% Worth noting that a common pattern in Erlang is to register a PID
%%% to a name, and then invoke a function by sending a message to that
%%% name.
handle_call(_, _, State) -> {noreply, State}.

handle_cast(send_title, ProxyPID) ->
  io:format("Handling send_title cast.~n", []),
  conversation:become(ProxyPID, ?CONV_REG_ATOM, "A", send_title, []),
  {noreply, ProxyPID}.

%%%%%%%% API Functions
%%% Worth noting that a common pattern in Erlang is to register a PID
%%% to a name, and then invoke a function by sending a message to that
%%% name.
start_link() ->
  ssa_gen_server:start_link({local, ?BUYER1}, dist_buyer1, [], []).

send_title() ->
  ssa_gen_server:cast(?BUYER1, send_title).

