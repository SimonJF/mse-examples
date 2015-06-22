-module(seller_sup).
-behaviour(supervisor).
-compile(export_all).

init(_Args) ->
  SellerProc = {sup_seller, {sup_seller,
                             start_link,
                             []},
                permanent, brutal_kill, worker, [sup_seller]},
  {ok, {{one_for_all, 2, 60}, [SellerProc]}}.

start_link() ->
  supervisor:start_link(seller_sup, []).
