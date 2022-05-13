let test_simple_invocation () =
  let code = {|
    (module
      (memory (export "memory") 1)
      (func (export "main") (param i32) (param i32) (result i32)
        local.get 1
        local.get 0
        i64.load
        local.get 1
        i64.load
        i64.add
        i64.store
        i32.const 8))
  |} in
  let storage =
    Hexdump.of_string {|
      01 00 00 00 00 00 00 00
    |}
  in
  let argument =
    Hexdump.of_string {|
      2A 00 00 00 00 00 00 00
    |}
  in
  let contract = 
    match Wasm_vm.Contract.make ~storage ~code with
    | Ok contract -> contract
    | Error msg -> Alcotest.fail msg
  in
  let runtime = Wasm_vm.Runtime.make ~contract in
  match Wasm_vm.Runtime.invoke runtime argument with
  | Ok storage -> Alcotest.(check Hexdump.hex) "Same" (Hexdump.of_string "2B 00 00 00 00 00 00 00") storage
  | Error msg -> Alcotest.fail msg

let test =
  let open Alcotest in
  "Runtime", [ test_case "Simple invocation" `Quick test_simple_invocation ]
