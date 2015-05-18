-module(sequenced_client).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting conversation in client.~n", []),
  conversation:start_conversation(Monitor, "SynchronousSequencedStateCells", "Client"),
  no_state.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_PN, _RN, _CID, ConvKey, State) ->
  actor_logger:info(client, "Get from state cell #1~n", []),
  Res1 = sequenced_state_cell:get(ConvKey, "StateCell1"),
  actor_logger:info(client, "Get #1 returned ~p~n", [Res1]),
  Res2 = sequenced_state_cell:get(ConvKey, "StateCell2"),
  actor_logger:info(client, "Get #2 returned ~p~n", [Res2]),
  ToPut = Res1 + Res2,
  actor_logger:info(client, "Putting ~p into result cell~n", [ToPut]),
  sequenced_state_cell:put(ConvKey, "StateCellRes", ToPut),
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(client, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(client, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_message(_, _, _, _, _, State, _Monitor) ->
  {ok, State}.

handle_info(Msg, State) ->
  io:format("Handle info msg: ~p~n", [Msg]),
  {noreply, State}.
handle_cast(Msg, State) ->
  io:format("Handle cast msg: ~p~n", [Msg]),
  {noreply, State}.



terminate(_, _) -> ok.
