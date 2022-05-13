let () =
  let open Alcotest in
  run "Wasm" [Test_parsing.test_parsing ; Test_vm .test]
