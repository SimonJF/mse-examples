-module(sequenced_state_cell).
-behaviour(ssa_gen_server).
-compile(export_all).

ssactor_init([InitialValue], Monitor) ->
  % Start the conversation.
  % False means we're not involved at the moment (used so that other
  % instances get invited)
  {false, InitialValue}.

ssactor_join(_, _, _, {false, Value}) -> {accept, {true, Value}};
ssactor_join(_, _, _, State) -> {decline, State}.

ssactor_conversation_established(_PN, _RN, _CID, _ConvKey, State) ->
  {ok, State}.

ssactor_conversation_error(_PN, _RN, Error, State) ->
  actor_logger:err(state_cell, "Could not establish conversation: ~p~n", [Error]),
  {ok, State}.

ssactor_conversation_ended(CID, _Reason, State) ->
  actor_logger:info(state_cell, "Conversation ~p ended.~n", [CID]),
  {ok, State}.

ssactor_handle_call(_, _, _, _, "put", [Value], _State, _ConvKey) ->
  actor_logger:info(state_cell, "Putting value ~p into state cell~n", [Value]),
  {reply, ok, {true, Value}};
ssactor_handle_call(_, _, _, _, "get", [], {_, Value}, _ConvKey) ->
  actor_logger:info(state_cell, "Retrieving value ~p from state cell~n", [Value]),
  {reply, Value, {true, Value}}.

ssactor_handle_message(_, _, _, _, _, State, _Monitor) ->
  {ok, State}.

handle_info(Msg, State) ->
  io:format("Handle info msg: ~p~n", [Msg]),
  {noreply, State}.
handle_cast(Msg, State) ->
  io:format("Handle cast msg: ~p~n", [Msg]),
  {noreply, State}.

code_change(_, _, State) -> {ok, State}.
terminate(_, _) -> ok.


% API
put(ConvKey, StateCellName, NewValue) ->
  conversation:call(ConvKey, StateCellName, "put", [], [NewValue]).

get(ConvKey, StateCellName) ->
  conversation:call(ConvKey, StateCellName, "get", [], []).
