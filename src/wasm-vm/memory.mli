type t = Wasm.Instance.memory_inst

val store_bytes : t -> address:int64 -> content:bytes -> unit

val load_bytes : t -> address:int64 -> size:int -> bytes

val store : t -> address:int64 -> content:string -> unit

val load : t -> address:int64 -> size:int -> string
