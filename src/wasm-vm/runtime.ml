open Wasm


type t = { contract : Contract.t
          ; instance : Instance.module_inst }

let make ~contract =
  let instance = Eval.init contract.Contract.module_ [] in
  { contract ; instance }

let memory_blit t address content =
  Memory.store_bytes t address (Bytes.to_string content)

let main = Utf8.decode "main"
let memory = Utf8.decode "memory"

let invoke t argument =
  let ( let* ) f r = Result.bind f r in
  let* memory =
    match Instance.export t.instance memory with
    | Some (ExternMemory memory) -> Ok memory
    | Some _ -> Error "Memory exported is not a memory"
    | None   -> Error "Should export a memory"
  in
  let* entrypoint =
    (* TODO: Verify entrypoint signature beforehand? *)
    match Instance.export t.instance main with
    | Some (ExternFunc func) -> Ok func
    | Some _ -> Error "Exported entrypoint is not a function"
    | None   -> Error "Entrypoint not found, should export a main function."
  in
  let argument, storage =
    let argument, storage =
      memory_blit memory 0L argument;
      (* It is trivial to have an arugment 0 when the argument position is known,
        * but then we can make sure that we can put the argument wherever we want to. *)
      (0L, Int64.of_int @@ Bytes.length argument)
    in
    memory_blit memory storage t.contract.storage;
    Int64.to_int32 argument, Int64.to_int32 storage
  in
  try
    (* TODO: Encode operations on the return *)
    match Eval.invoke entrypoint [ Value.i32 argument ; Value.i32 storage ] with
    | [ Values.Num (I32 size) ] ->
      Ok (Bytes.of_string (Memory.load_bytes memory (Int64.of_int32 storage) (Int32.to_int size)))
    | _ -> Error "Wrong return type"
  with
  | Eval.Crash (_, error)
  | Eval.Exhaustion (_, error)
  | Eval.Trap (_, error) -> Error error
