-module(client).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting conversation in client.~n", []),
  conversation:start_conversation(Monitor, "StateCell", "Client"),
  no_state.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_PN, _RN, _CID, ConvKey, State) ->
  actor_logger:info(client, "Put ~p into state cell~n", ["hello"]),
  Res = conversation:call(ConvKey, "StateCell", "put", ["String"], ["hello"]),
  io:format("After the synchronous call, got result ~p~n", [Res]),
  actor_logger:info(client, "Get #1 from state cell~n", []),
  Res1 = conversation:call(ConvKey, "StateCell", "get", [], []),
  actor_logger:info(client, "Get #1 returned ~p~n", [Res1]),
  actor_logger:info(client, "Put ~p into state cell~n", ["world"]),
  ok = conversation:call(ConvKey, "StateCell", "put", ["String"], ["world"]),
  actor_logger:info(client, "Get #2 from state cell~n", []),
  Res2 = conversation:call(ConvKey, "StateCell", "get", [], []),
  actor_logger:info(client, "Get #1 returned ~p~n", [Res2]),
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
