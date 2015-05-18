-module(async_client).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  io:format("Starting conversation in client.~n", []),
  conversation:start_conversation(Monitor, "SequencedStateCells", "Client"),
  0.


ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(_PN, _RN, _CID, ConvKey, _State) ->
  actor_logger:info(client, "Get from state cell #1~n", []),
  Res1 = async_state_cell:get_request(ConvKey, "StateCell1"),
  {ok, 0}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(client, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.


ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(client, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

% Note that each response requires a separate handler
ssactor_handle_message(_, _, _, "StateCell1", "get_response", [Res1], _, ConvKey) ->
  actor_logger:info(client, "Get #1 returned ~p~n", [Res1]),
  async_state_cell:get_request(ConvKey, "StateCell2"),
  {ok, Res1};
ssactor_handle_message(_, _, _, "StateCell2", "get_response", [Res2], State, ConvKey) ->
  actor_logger:info(client, "Get #2 returned ~p~n", [Res2]),
  ToPut = State + Res2,
  actor_logger:info(client, "Putting ~p into result cell~n", [ToPut]),
  async_state_cell:put_request(ConvKey, "StateCellRes", ToPut),
  {ok, ToPut};
ssactor_handle_message(_, _, _, "StateCellRes", "put_response", _, State, _ConvKey) ->
  {ok, State}.

ssactor_handle_call(_, _, _, _, _, _, State, _) -> {ok, State}.

handle_info(Msg, State) ->
  io:format("Handle info msg: ~p~n", [Msg]),
  {noreply, State}.
handle_cast(Msg, State) ->
  io:format("Handle cast msg: ~p~n", [Msg]),
  {noreply, State}.



terminate(_, _) -> ok.


%%% API
put_response(ConvKey) ->
  conversation:send(ConvKey, ["Client"], "put_response", [], [ok]).

get_response(ConvKey, Value) ->
  conversation:send(ConvKey, ["Client"], "get_response", [], [Value]).
