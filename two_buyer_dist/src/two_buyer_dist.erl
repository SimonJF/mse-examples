-module(two_buyer_dist).
-compile(export_all).
-behaviour(application).

start(normal, _Args) ->
  application:start(monitored_session_erlang),
  conversation:initialise("../standalone/scribble_specs", two_buyer_conf:config()).

stop(_) -> ok.
