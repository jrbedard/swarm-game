{application, swarm,
 [{description, "swarm"},
  {vsn, "0.01"},
  {modules, [
    swarm,
    swarm_app,
    swarm_sup,
    swarm_web,
    swarm_deps
  ]},
  {registered, []},
  {mod, {swarm_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
