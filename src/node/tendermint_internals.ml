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