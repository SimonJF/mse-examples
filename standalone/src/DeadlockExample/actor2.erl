-module(actor2).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting sessions in actor 2.~n", []),
  conversation:start_conversation(Monitor, "M", "B"),
  conversation:start_conversation(Monitor, "N", "D"),
  no_state.


ssactor_join(_, Role, _, State) ->
  io:format("actor2 joining as ~p~n", [Role]),
  {accept, State}.

ssactor_conversation_established("M", _RN, _CID, ConvKey, State) ->
  conversation:register_conversation(m_session, ConvKey),
  {ok, State};
ssactor_conversation_established("N", _RN, _CID, ConvKey, State) ->
  conversation:register_conversation(n_session, ConvKey),
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(actor2, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(actor2, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("N", "D", _, _, "Y", _, State, ConvKey) ->
  actor_logger:info(actor2, "Received message X in N as D", []),
  conversation:become(ConvKey, m_session, "C", send_in_m, []),
  {ok, State};
ssactor_handle_message(_, _, _CID, _SenderRole, Op, Payload, _State, _ConvKey) ->
  actor_logger:err(actor2, "Unhandled message: (~s, ~w)", [Op, Payload]),
  {ok, no_state}.


ssactor_become("M", "A", send_in_m, _, ConvKey, State) ->
  conversation:send(ConvKey, ["B"], "X", []),
  {ok, State}.

ssactor_subsession_complete(_, _, State, _) -> {stop, unexpected_become, State}.
ssactor_subsession_failed(_, _, State, _) -> {ok, State}.
ssactor_subsession_setup_failed(_, _, State, _) -> {ok, State}.

handle_call(_, _, State) -> {stop, unexpected_call, State}.
handle_cast(_, State) -> {stop, unexpected_cast, State}.
handle_info(_, State) -> {stop, unexpected_info, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.
