{application, mse_examples,
  [{vsn, "0.1"},
  {modules, [two_buyer_main, buyer2, buyer1, two_buyer_conf, seller,
             simple_sup, supervised_actor2, simple_supervisor_main,
             simple_supervisor_conf, supervised_actor1, warehouse, dealer,
             warehouse_main, warehouse_conf, customer, actorB, actorC, actorA,
             delayed_test_main, delayed_test_conf, actor_logger, two_buyer_dist,
             dist_buyer1, dist_buyer2, dist_seller]},
  {registered, []}
]}.
