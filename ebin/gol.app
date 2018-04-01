{application, gol,
 [{description, "Game Of Life"},
  {vsn, "1.0"},
  {modules, [gol_app, gol_sup, gol_api, gol_cfg, gol_cell_sup, gol_cell]},
  {registered, [gol_app,
                gol_sup,
                gol_cfg,
                gol_cell_sup
               ]},
  {applications, [kernel,
                  stdlib]},
  {mod, {gol_app, []}}
 ]
}.
