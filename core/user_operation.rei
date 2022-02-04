open Crypto;

type initial_operation =
  | Single_operation(string);

[@deriving (eq, ord, yojson)]
type t =
  pri {
    hash: BLAKE2B.t,
    sender: Address.t,
    initial_operation,
  };

let make: (~sender: Address.t, initial_operation) => t;
