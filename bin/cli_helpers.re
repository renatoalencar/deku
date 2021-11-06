open Cmdliner;

// Log helpers
// We take logging arguments on the command line (e.g. -v to enable verbosity)
// So instead of writing e.g. `const(sign_block) $ folder_node $ block_hash`,
// You would write `const_log(sign_block) $ folder_node $ block_hash` to enable
// logging support.
let setup_log = (style_renderer, level) => {
  switch (style_renderer) {
  | Some(style_renderer) => Fmt_tty.setup_std_outputs(~style_renderer, ())
  | None => Fmt_tty.setup_std_outputs()
  };

  Logs.set_level(level);
  Logs.set_reporter(Logs_fmt.reporter());
  ();
};
let setup_log =
  Term.(const(setup_log) $ Fmt_cli.style_renderer() $ Logs_cli.level());

let const_log = p => Term.(const(() => p) $ setup_log);
