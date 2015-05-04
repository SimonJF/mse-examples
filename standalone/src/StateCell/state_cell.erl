-module(state_cell).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init(_Args, Monitor) ->
  % Start the conversation
  "".

ssactor_join(_, _, _, State) -> {accept, State}.

ssactor_conversation_established(PN, RN, _CID, _ConvKey, State) ->
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(state_cell, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.

ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(state_cell, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_call("StateCell", "StateCell", _, _, "put", [Value], _State, _ConvKey) ->
  actor_logger:info(state_cell, "Putting value ~p into state cell~n", [Value]),
  {reply, ok, Value};
ssactor_handle_call("StateCell", "StateCell", _, _, "get", [], Value, _ConvKey) ->
  actor_logger:info(state_cell, "Retrieving value ~p from state cell~n", [Value]),
  {reply, Value, Value}.

ssactor_handle_message(_, _, _, _, _, State, _Monitor) ->
  {ok, State}.

handle_info(Msg, State) ->
  io:format("Handle info msg: ~p~n", [Msg]),
  {noreply, State}.
handle_cast(Msg, State) ->
  io:format("Handle cast msg: ~p~n", [Msg]),
  {noreply, State}.

terminate(_, _) -> ok.
