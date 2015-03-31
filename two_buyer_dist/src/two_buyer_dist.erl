-compile(export_all).
-module(two_buyer_dist).
-behaviour(application).

start(normal, _Args) ->
  conversation:initialise("../standalone/scribble_specs", two_buyer_conf:config()).

stop(_) -> ok.
