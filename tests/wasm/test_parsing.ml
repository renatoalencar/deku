exception Parse_error

let parse code =
  match Wasm_vm.Module.of_string ~code with
  | Ok module_ -> module_
  | Error _ -> raise Parse_error

let get_entrypoint t =
  match Wasm.Instance.export t (Wasm.Utf8.decode "entrypoint") with
  | Some (ExternFunc func) -> Ok func
  | Some _ -> Error `Execution_error
  | None -> Error `Execution_error

let get_memory t =
  match Wasm.Instance.export t (Wasm.Utf8.decode "memory") with
  | Some (ExternMemory func) -> Ok func
  | Some _ -> Error `Execution_error
  | None -> Error `Execution_error

let test_successful_parsing () =
  let d = Ex.x in
  let _ =
    let inst = Wasm.Eval.init (ref max_int) d [] in
    let func = get_entrypoint inst |> Result.get_ok in
    Wasm.Eval.invoke (ref max_int) func
      [
        Wasm.Values.Num (I32 (Int32.of_int 55));
        Wasm.Values.Num (I32 (Int32.of_int 55));
      ]
    |> List.hd
    |> function
    | Wasm.Values.Num (I32 _res) ->
      let _mem = get_memory inst |> Result.get_ok in
      let tup =
        Wasm.Memory.load_bytes _mem
          (Int64.of_int32 @@ Int32.add _res (Int32.of_int 4))
          4
        |> Bytes.of_string in
      let _ok = Bytes.get_int32_le tup 0 in
      let tup =
        Wasm.Memory.load_bytes _mem (Int64.of_int32 _ok) 4 |> Bytes.of_string
      in
      let _ok = Bytes.get_int32_le tup 0 in
      Format.printf "%ld" _ok;
      let _ok = _ok = Int32.of_int 0 in
      assert (_ok = true)
    | _ -> assert false in
  let code =
    {|
    (module
      (import "env" "syscall" (func $syscall (param i64) (result i32)))
      (memory $mem 1)
      (export "memory" (memory $mem))
      (type $sum_t (func (param i32 i32) (result i32)))
      (type $main_t (func (param i32) (result i64 i64 i64)))
      (func $sum_f (type $sum_t) (param $x i32) (param $y i32) (result i32)
        i32.const 0
        local.get $x
        local.get $y
        i32.add
        i32.store
        i32.const 0
        i32.load)
      (func $main (type $main_t) (param i32) (result i64 i64 i64) 
         i64.const 0
         i64.const 0
         i64.const 0)
      (export "main" (func $main)))
    |}
  in
  ignore (parse code)

let test_incorrect_syntax () =
  let code = "(module" in
  Alcotest.check_raises "Raises parse error" Parse_error (fun () ->
      ignore (parse code))

let test_memory_size () =
  let code = "(module (memory 65540))" in
  Alcotest.check_raises "Raises validation error" Parse_error (fun () ->
      ignore (parse code))

let test_multiple_memories () =
  let code = "(module (memory 1) (memory 1))" in
  Alcotest.check_raises "Raises validation error" Parse_error (fun () ->
      ignore (parse code))

let test_func_validation () =
  let code = "(module (func $main (result i32) i64.const 0))" in
  Alcotest.check_raises "Raises validation error" Parse_error (fun () ->
      ignore (parse code))

let test =
  let open Alcotest in
  ( "Parsing",
    [
      test_case "Successful parsing" `Quick test_successful_parsing;
      test_case "Error on incorrect syntax" `Quick test_incorrect_syntax;
      test_case "Contract validation (memory size)" `Quick test_memory_size;
      test_case "Contract validation (single memory)" `Quick
        test_multiple_memories;
      test_case "Contract validation (function)" `Quick test_func_validation;
    ] )
