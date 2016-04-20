-module(reflexive_buyer).
-behaviour(ssa_gen_server).
-compile(export_all).
-define(PRICE_THRESHOLD, 50).

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting conversation in reflexive_buyer.~n", []),
  conversation:start_conversation(Monitor, "TwoBuyers", "A"),
  no_state.


ssactor_join(_, Role, _, State) ->
  io:format("reflexive_buyer joining as ~p~n", [Role]),
  {accept, State}.

ssactor_conversation_established(PN, RN, _CID, ConvKey, State) ->
  if PN == "TwoBuyers" andalso RN == "A" ->
      conversation:send(ConvKey, ["S"], "title", ["String"], ["To Kill a Mockingbird"]);
     true -> ok
  end,
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(reflexive_buyer, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(reflexive_buyer, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "quote", [QuoteInt], State, ConvKey) ->
  actor_logger:info(reflexive_buyer, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  conversation:send(ConvKey, ["B"], "share", ["Integer"], [QuoteInt div 2]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "accept", [Address], State, _ConvKey) ->
  actor_logger:info(reflexive_buyer, "~s accepted quote; received address (~p)", [SenderRole, Address]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "retry", _, State, _ConvKey) ->
  actor_logger:info(reflexive_buyer, "~s wants to retry", [SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "A", _, SenderRole, "quit", _, State, _ConvKey) ->
  actor_logger:info(reflexive_buyer, "~s wants to quit", [SenderRole]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "A", _CID, _SenderRole, Op, Payload, State, _ConvKey) ->
  actor_logger:err(reflexive_buyer, "Unhandled message: (~s,  ~w)", [Op, Payload]),
  {ok, State};
ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "quote", [QuoteInt], _State, _ConvKey) ->
  actor_logger:info(reflexive_buyer, "Received quote of ~p from ~s", [QuoteInt, SenderRole]),
  {ok, no_state};
ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "share", [Share], _State, ConvKey) ->
  actor_logger:info(reflexive_buyer, "Received share quote (~p) from ~s", [Share, SenderRole]),
  if Share >= ?PRICE_THRESHOLD ->
       % Nah, we aint paying that
       actor_logger:info(reflexive_buyer, "Rejected share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(ConvKey, ["A", "S"], "quit", [], []);
     Share < ?PRICE_THRESHOLD ->
       % We can afford it: accept, send address to reflexive_buyer and server,
       % and retrieve the delivery date from the server
       actor_logger:info(reflexive_buyer, "Accepted share quote (threshold ~p)", [?PRICE_THRESHOLD]),
       conversation:send(ConvKey, ["A", "S"], "accept",
                         ["String"], ["Informatics Forum"])
  end,
  {ok, no_state};
ssactor_handle_message("TwoBuyers", "B", _CID, SenderRole, "date", [DeliveryDate], _State, ConvKey) ->
  actor_logger:info(reflexive_buyer, "Received delivery date of ~s from ~s", [DeliveryDate, SenderRole]),
  conversation:end_conversation(ConvKey, normal),
  {ok, no_state};
ssactor_handle_message("TwoBuyers", "B", _CID, _SenderRole, Op, Payload, _State, _ConvKey) ->
  actor_logger:err(reflexive_buyer, "Unhandled message: (~s, ~w)", [Op, Payload]),
  {ok, no_state}.


ssactor_become(_, _, _, _, State) -> {stop, unexpected_become, State}.
ssactor_subsession_complete(_, _, State, _) -> {stop, unexpected_become, State}.
ssactor_subsession_failed(_, _, State, _) -> {ok, State}.
ssactor_subsession_setup_failed(_, _, State, _) -> {ok, State}.

handle_call(_, _, State) -> {stop, unexpected_call, State}.
handle_cast(_, State) -> {stop, unexpected_cast, State}.
handle_info(_, State) -> {stop, unexpected_info, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.
