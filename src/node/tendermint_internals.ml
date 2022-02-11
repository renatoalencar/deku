open Protocol

(** Tendermint sometimes decides on a `Nil` value. *)
type value =
  | Block of Protocol.Block.t
  | Nil
[@@deriving yojson]

let string_of_value = function
  | Nil -> "nil"
  | Block b ->
    Printf.sprintf "block %s" (Crypto.BLAKE2B.to_string b.Protocol.Block.hash)

(* TODO: FIXME: Tendermint *)
let repr_of_value v = v

(* FIXME: Tendermint (this is copied bad design)*)
let produce_value : (State.t -> value) ref = ref (fun _ -> assert false)

let block b = Block b
let nil = Nil

type height = int64 [@@deriving yojson]

type round = int [@@deriving yojson]
(** At a specific (chain) height, Tendermint's consensus algorithm may run several rounds.*)

(** Tendermint's consensus goes through 3 steps at each round. Used inside a consensus instance in a node. *)
type consensus_step =
  | Proposal
  | Prevote
  | Precommit

(** Tendermint's consensus step-communication with other nodes. *)
type sidechain_consensus_op =
  | ProposalOP  of (height * round * value * round)
  | PrevoteOP   of (height * round * value)
  | PrecommitOP of (height * round * value)
[@@deriving yojson]

let step_of_op = function
  | ProposalOP _ -> Proposal
  | PrevoteOP _ -> Prevote
  | PrecommitOP _ -> Precommit

let string_of_op = function
  | ProposalOP (height, round, value, vround) ->
    Printf.sprintf "<PROPOSAL, %Ld, %d, %s, %d>" height round
      (string_of_value value) vround
  | PrevoteOP (height, round, value) ->
    Printf.sprintf "<PREVOTE, %Ld, %d, %s>" height round (string_of_value value)
  | PrecommitOP (height, round, value) ->
    Printf.sprintf "<PRECOMMIT, %Ld, %d, %s>" height round
      (string_of_value value)

let height = function
  | ProposalOP (h, _, _, _)
  | PrevoteOP (h, _, _)
  | PrecommitOP (h, _, _) ->
    h
