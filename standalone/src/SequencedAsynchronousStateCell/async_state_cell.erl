-module(async_state_cell).
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

ssactor_handle_call(_, _, _, _, _, _, State, _) ->
  {noreply, State}.

ssactor_handle_message(_, _, _, _, "put_request", [Value], _State, ConvKey) ->
  async_client:put_response(ConvKey),
  {ok, {true, Value}};
ssactor_handle_message(_, _, _, _, "get_request", [], State = {_, Value}, ConvKey) ->
  async_client:get_response(ConvKey, Value),
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
put_request(ConvKey, StateCellName, NewValue) ->
  conversation:send(ConvKey, [StateCellName], "put_request", [], [NewValue]).

get_request(ConvKey, StateCellName) ->
  conversation:send(ConvKey, [StateCellName], "get_request", [], []).
