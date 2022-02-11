open Tendermint_internals
open Tendermint_data
open Tendermint_processes
open Tendermint_helpers

module CI = Tendermint_internals

type t = {
  identity : State.identity;
  (* FIXME: clocks *)
  consensus_states : consensus_state IntSet.t;
  procs : (height * process) list;
  node_state : State.t;
  input_log : input_log;
  output_log : output_log;
}
(** Tendermint simplification: as Deku is not going to run several
    blockchain heights at the same time, we only consider one set of
    states and clocks. *)

let make identity node_state current_height =
  (* FIXME: add clocks *)
  let new_state = fresh_state current_height in
  let states = IntSet.create 0 in
  IntSet.add states 0L new_state;
  let procs = List.map (fun x -> (current_height, x)) all_processes in
  let input_log = empty () in
  let output_log = OutputLog.empty () in
  {
    identity;
    consensus_states = states;
    node_state;
    procs;
    input_log;
    output_log;
  }

let current_height node =
  List.fold_left max 0L (IntSet.to_seq_keys node.consensus_states |> List.of_seq)

(** Process messages in the queue and decide actions; pure function, to be interpeted in Lwt later.
    TODO: Ensures that messages received over network go through signature verification before adding them to input_log
    FIXME: we're not empyting the input_log atm *)
let tendermint_step node =
  let rec exec_procs processes still_active network_actions =
    match processes with
    | ((height, process) as p) :: rest -> begin
      let consensus_state = IntSet.find node.consensus_states height in
      let round = consensus_state.round in
      let message_log = node.input_log in
      match
        process height round consensus_state message_log (OutputLog.empty ())
          node.node_state
      with
      (* The process precondition hasn't been activated *)
      | None -> exec_procs rest (p :: still_active) network_actions
      (* The process terminates with a network event *)
      | Some (Broadcast t) -> exec_procs rest still_active (t :: network_actions)
      (* The process terminates silently *)
      | Some DoNothing -> exec_procs rest still_active network_actions
      (* We accepted a block for the height *)
      | Some TendermintComplete ->
        (* Start new processes and forget about the older ones *)
        ([], network_actions)
      (* Add a new clock to the scheduler *)
      | Some Schedule (* FIXME: clocks *) ->
        (* FIXME: handle clock *)
        exec_procs rest still_active network_actions
    end
    | [] -> (still_active, network_actions) in
  prerr_endline
    (Printf.sprintf "*** About to execute %d processes" (List.length node.procs));
  let still_active, network_actions = exec_procs node.procs [] [] in
  (* TODO: Do we really need order on the network? *)
  ({ node with procs = still_active }, List.rev network_actions)

let add_to_input node sender op =
  let input_log = node.input_log in
  let index = (height op, step_of_op op) in
  add input_log index (content_of_op sender op)

let is_valid_consensus_op state consensus_op =
  (* TODO: this is a filter for input log since it's only optimization (don't keep stuff from past)*)
  let open Result in
  let _all_operations_properly_signed = function
    | _ -> true in
  let h = CI.height consensus_op in
  let current_height = state.State.protocol.block_height in
  if current_height > h then
    let s =
      Printf.sprintf
        "new block has a lower height (%Ld) than the current state (%Ld)" h
        current_height in
    error s
  else
    ok ()

let broadcast_op state consensus_op =
  prerr_endline "*** Called broadcast";
  (* TODO: Network stuff *)
  prerr_endline "*** Broadcasted"

let add_consensus_op node update_state sender op =
  let input_log = add_to_input node sender op in
  (* TODO: Call tendermint_step here? Call update_state_here? *)
  { node with input_log }

let rec exec_consensus node =
  let open CI in
  let node, network_actions = tendermint_step node in
  List.iter (broadcast_op node.node_state) network_actions;
  (*TODO: clocks?*)
  match node.procs with
  (* If we no longer have any active process, we start next height!*)
  | [] ->
    let cur_height = current_height node in
    let new_height = Int64.add cur_height 1L in
    let new_state = CI.fresh_state new_height in
    IntSet.add node.consensus_states new_height new_state;
    let new_processes = List.map (fun p -> (new_height, p)) all_processes in
    exec_consensus { node with procs = new_processes }
  | _ -> node

let make_proposal height round block =
  CI.ProposalOP (height, round, CI.block block, -1)
