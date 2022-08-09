open Wasm

type t = Memory.t

let store_bytes t ~address ~content =
  Memory.store_bytes t address (Bytes.to_string content)

let load_bytes t ~address ~size =
  let loaded = Memory.load_bytes t address size in
  Bytes.of_string loaded

let load t ~address ~size = Memory.load_bytes t address size

let store t ~address ~content = Memory.store_bytes t address content
