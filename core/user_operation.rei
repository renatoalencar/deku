open Crypto;

type initial_operation =
  | Single_operation(Yojson.Safe.t);

[@deriving (eq, ord, yojson)]
type t =
  pri {
    hash: BLAKE2B.t,
    sender: Address.t,
    initial_operation,
  };

let make: (~sender: Address.t, initial_operation) => t;
