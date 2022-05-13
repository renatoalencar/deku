open Wasm

type t = Values.value

let i32 v =
  Values.Num (I32 v)

let i64 v =
  Values.Num (I64 v)

let f32 v =
  Values.Num (F32 v)

let f64 v =
  Values.Num (F64 v)

let to_int32 = function
| Values.Num (I32 v) -> Some v
| _ -> None

let to_int64 = function
| Values.Num (I64 v) -> Some v
| _ -> None

let to_f32 = function
| Values.Num (F32 v) -> Some v
| _ -> None

let to_f64 = function
| Values.Num (F64 v) -> Some v
| _ -> None
