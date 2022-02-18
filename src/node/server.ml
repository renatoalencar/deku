open Helpers
type t = {
  mutable state : State.t;
  mutable consensus : Tendermint.t;
  mutable timeout : unit Lwt.t;
}
let global_server = ref None
let start ~initial =
  let consensus =
    Tendermint.make initial (Int64.add initial.protocol.block_height 1L)
  in
  match !global_server with
  | Some _ -> failwith "start should be called just once"
  | None ->
    global_server :=
      Some { state = initial; consensus; timeout = Lwt.return_unit }
let get () =
  match !global_server with
  | Some state -> state
  | None -> failwith "get called before start"
let get_port () = (get ()).state.identity.uri |> Uri.port

let get_consensus () = (get ()).consensus
let set_consensus consensus = (get ()).consensus <- consensus
let get_state () = (get ()).state
let set_state state =
  let server_state = get () in
  let current_consensus = server_state.consensus in
  server_state.state <- state;
  server_state.consensus <- { current_consensus with Tendermint.node_state = state }

let _ = Flows.get_state := get_state
let _ = Flows.set_state := set_state
let _ = Flows.get_consensus := get_consensus
let _ = Flows.set_consensus := set_consensus
