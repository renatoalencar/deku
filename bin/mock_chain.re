open Helpers;
open Crypto;
open Core;

let initial_state = Core.State.empty |> String_map.add("counter", `Int(0));

let (_, sender) = Address.make();

let _ = Named_pipe.make_pipe_pair("/tmp/state_transition");

let () = print_endline("hello");
let _ = Named_pipe.get_pipe_pair_file_descriptors("/tmp/state_transition");

let rec main = t => {
  let action = Yojson.Safe.from_string({|{"Action":"Increment"}|});
  open User_operation;
  let operation =
    User_operation.make(
      ~sender,
      Single_operation(action),
    );
  Unix.sleep(1);
  Format.printf(
    "Current counter state: %s\n%!",
    String_map.find("counter", t) |> Yojson.Safe.to_string,
  );
  main(State.apply_user_operation(t, operation));
};

let _ = main(initial_state);
