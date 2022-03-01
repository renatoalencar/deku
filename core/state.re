open Helpers;
open Crypto;

let path_ref = ref("");

let start_state_transition_machine = (~path) => {
  let path = path ++ "/state_transition";
  path_ref := path;
  let _pid =
    Unix.create_process(
      path,
      [|path, path|],
      Unix.stdin,
      Unix.stdout,
      Unix.stderr,
    );
  ();
};

let read_all = (fd, length) => {
  let message = Bytes.create(length);
  let pos = ref(0);
  while (length > pos^) {
    let read = Unix.read(fd, message, pos^, length);
    pos := pos^ + read;
  };
  message;
};

let write_all = (fd, bytes_) => {
  let bytes_len = Bytes.length(bytes_);
  let remaining = ref(bytes_len);

  while (remaining^ > 0) {
    let pos = Bytes.length(bytes_) - remaining^;
    let wrote = Unix.write(fd, bytes_, pos, bytes_len);
    remaining := remaining^ - wrote;
  };
  Format.printf("finished writing message\n%!");
};

[@deriving yojson]
type t = String_map.t(Yojson.Safe.t);

let empty = String_map.empty |> String_map.add("counter", `Int(0));

let hash = t => to_yojson(t) |> Yojson.Safe.to_string |> BLAKE2B.hash;

[@deriving yojson]
type machine_message =
  | Stop
  | Set({
      key: string,
      value: Yojson.Safe.t,
    })
  | Get(string);

let send_to_machine = (message: Yojson.Safe.t) => {
  let (_, write_fd) = Named_pipe.get_pipe_pair_file_descriptors(path_ref^);

  Format.printf("Sending message: %s\n%!", message |> Yojson.Safe.to_string);
  let message = Bytes.of_string(Yojson.Safe.to_string(message));
  // First packet is a 64-bit integer specifying the number of bytes about to be sent.
  let message_length = Bytes.create(8);
  Bytes.set_int64_ne(
    message_length,
    0,
    Int64.of_int(Bytes.length(message)),
  );
  // Since the first packet is exactly 64 bits, it always fits in a single write.
  let _ =
    Unix.write(write_fd, message_length, 0, Bytes.length(message_length));
  // The following packets are the content of the message.
  write_all(write_fd, message);
};

let read_from_machine = () => {
  let (read_fd, _) =
    Named_pipe.get_pipe_pair_file_descriptors(path_ref^);
  let message_length = Bytes.create(8);
  // First packet is always a 64-bit integer specifying the number of bytes about to be sent.
  let _ = Unix.read(read_fd, message_length, 0, 8);
  let message_length = Bytes.get_int64_ne(message_length, 0) |> Int64.to_int;
  let message = read_all(read_fd, message_length) |> Bytes.to_string;
  Format.printf("Got message from machine: %s\n%!", message);
  let message = Yojson.Safe.from_string(message);
  let message = machine_message_of_yojson(message);
  Result.get_ok(message);
};

// call out to the go program here.
let apply_user_operation = (t, user_operation) => {
  let User_operation.{initial_operation: Single_operation(payload), _} = user_operation;
  send_to_machine(payload);
  let finished = ref(false);
  let state = ref(t);
  while (! finished^) {
    switch (read_from_machine()) {
    | Stop => finished := true
    | Set({key, value}) => state := String_map.add(key, value, state^)
    | Get(key) =>
      let value =
        String_map.find_opt(key, state^) |> Option.value(~default=`Null);
      send_to_machine(value);
    };
  };
  state^;
};
