-module(sup_buyer1).
-behaviour(ssa_gen_server).
-compile(export_all).

% Buyer 1:
%   Buyer 1 -> Server (title(String))
%   Server -> Buyer 1 (quote(Int))
%   Buyer 1 -> Buyer 2 (share(Int))
%   Choice:
%     Buyer2 -> Buyer 1 (accept(String))
%     Buyer2 -> Buyer 1 (retry())
%     Buyer2 -> Buyer 1 (quit())

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  start_new_conversation(Monitor),
  Monitor.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  if PN == "TwoBuyers" andalso RN == "A" ->
      conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]);
     true -> ok
  end,
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(buyer1, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(buyer1, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "quote", [QuoteInt], State, ConvKey) ->
  actor_logger:info(buyer1, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  conversation:send(ConvKey, ["B"], "share", ["Integer"], [QuoteInt div 2]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "accept", [Address], State, _ConvKey) ->
  actor_logger:info(buyer1, "~s accepted quote; received address (~p)", [SenderRole, Address]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "retry", _, State, _ConvKey) ->
  actor_logger:info(buyer1, "~s wants to retry", [SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "quit", _, State, _ConvKey) ->
  actor_logger:info(buyer1, "~s wants to quit", [SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "A", _CID, _SenderRole, Op, Payload, State, _ConvKey) ->
  actor_logger:err(buyer1, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, State}.

handle_cast(start_conversation, Monitor) ->
  io:format("Starting conversation in buyer1.~n", []),
  conversation:start_conversation(Monitor, "TwoBuyers", "A"),
  {noreply, Monitor}.


start_new_conversation(PID) ->
  ssa_gen_server:cast(PID, start_conversation).

terminate(_, _) -> ok.
