open Crypto;

[@deriving yojson]
// FIXME: make this a key value store
type t = unit;

let empty = ();

let hash = t => to_yojson(t) |> Yojson.Safe.to_string |> BLAKE2B.hash;

// call out to the go program here.
let apply_user_operation = (t, _user_operation) => t;
