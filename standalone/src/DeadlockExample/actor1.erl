-module(actor1).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  no_state.


ssactor_join(_, Role, _, State) ->
  io:format("actor1 joining as ~p~n", [Role]),
  {accept, State}.

ssactor_conversation_established("M", _RN, _CID, ConvKey, State) ->
  conversation:register_conversation(m_session, ConvKey),
  {ok, State};
ssactor_conversation_established("N", _RN, _CID, ConvKey, State) ->
  conversation:register_conversation(n_session, ConvKey),
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(actor1, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(actor1, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message("M", "B", _, _, "X", _, State, ConvKey) ->
  actor_logger:info(actor1, "Received message X in M as B", []),
  conversation:become(ConvKey, n_session, "C", send_in_n, []),
  {ok, State};
ssactor_handle_message(_, _, _CID, _SenderRole, Op, Payload, _State, _ConvKey) ->
  actor_logger:err(actor1, "Unhandled message: (~s, ~w)", [Op, Payload]),
  {ok, no_state}.


ssactor_become("N", "C", send_in_n, _, ConvKey, State) ->
  conversation:send(ConvKey, ["D"], "Y", []),
  {ok, State}.

ssactor_subsession_complete(_, _, State, _) -> {stop, unexpected_become, State}.
ssactor_subsession_failed(_, _, State, _) -> {ok, State}.
ssactor_subsession_setup_failed(_, _, State, _) -> {ok, State}.

handle_call(_, _, State) -> {stop, unexpected_call, State}.
handle_cast(_, State) -> {stop, unexpected_cast, State}.
handle_info(_, State) -> {stop, unexpected_info, State}.
code_change(_, State, _) -> {ok, State}.
terminate(_, _) -> ok.
