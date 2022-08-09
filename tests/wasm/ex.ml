open Wasm

let input_binary name buf =
  let open Wasm.Script in
  let open Source in
  Module (None, Encoded (name, buf) @@ no_region) @@ no_region

let input_binary_file file =
  let ic = open_in_bin file in
  try
    let len = in_channel_length ic in
    let buf = Bytes.make len '\x00' in
    really_input ic buf 0 len;
    let success = input_binary file (Bytes.to_string buf) in
    close_in ic;
    success
  with
  | exn ->
    close_in ic;
    raise exn

let x =
  match input_binary_file "./simple_invocation.mligo.wasm" with
  | { it = Module (_, { it = Script.Encoded (_, buf); at = _ }); at = _ } ->
    Decode.decode "" buf
  | _ -> assert false
