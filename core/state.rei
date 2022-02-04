open Crypto;

[@deriving yojson]
type t;

let empty: t;
let hash: t => BLAKE2B.t;

let apply_user_operation: (t, User_operation.t) => t;
