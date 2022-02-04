type blake2b = bytes

(* store hash *)
type validator = key_hash
type validators = validator list
type validator_key = key
type validator_keys = validator_key option list

(* Root_hash_update contract *)
type root_hash_storage = {
  (* TODO: is having current_block_hash even useful? *)
  (* consensus proof *)
  current_block_hash: blake2b;
  current_block_height: int;
  current_state_hash: blake2b;
  current_validators: validators;
}

type signatures = signature option list

type root_hash_action = {
  block_height: int;
  block_payload_hash: blake2b;

  state_hash: blake2b;
  (* TODO: performance, can this blown up? *)
  validators: validators;

  current_validator_keys: validator_keys;
  signatures: signatures;
}

(* (pair (pair int bytes) (pair bytes validators)) *)
(* TODO: performance, put this structures in an optimized way *)
type block_hash_structure = {
  block_height: int;
  block_payload_hash: blake2b;
  state_hash: blake2b;
  validators_hash: blake2b;
}

let assert_msg ((message, condition): (string * bool)) =
  if not condition then
    failwith message

let root_hash_check_block_height
  (storage: root_hash_storage)
  (block_height: int) =
    assert_msg (
      "old block height",
      block_height > storage.current_block_height
    )

let root_hash_block_hash (root_hash_update: root_hash_action) =
  let block_hash_structure = {
    block_height = root_hash_update.block_height;
    block_payload_hash = root_hash_update.block_payload_hash;
    state_hash = root_hash_update.state_hash;
    (* TODO: should we do pack of list? *)
    validators_hash = Crypto.blake2b (Bytes.pack root_hash_update.validators)
  } in
  Crypto.blake2b (Bytes.pack block_hash_structure)

let rec root_hash_check_keys
  (validator_keys, validators, block_hash, remaining:
    validator_keys * validators * blake2b * int) : unit =
    match (validator_keys, validators) with
    | ([], []) ->
      if remaining > 0 then
        failwith "not enough keys"
    | ((Some validator_key :: vk_tl), (validator :: v_tl)) ->
      if (Crypto.hash_key validator_key) = validator then
        root_hash_check_keys (vk_tl, v_tl, block_hash, (remaining - 1))
      else failwith "validator_key does not match validator hash"
    | ((None :: vk_tl), (_ :: v_tl)) ->
      root_hash_check_keys (vk_tl, v_tl, block_hash, remaining)
    | (_, _) ->
      failwith "validator_keys and validators have different size"


let rec root_hash_check_signatures
  (validator_keys, signatures, block_hash, remaining:
    validator_keys * signatures * blake2b * int) : unit =
    match (validator_keys, signatures) with
    (* already signed *)
    | ([], []) ->
      (* TODO: this can be short circuited *)
      if remaining > 0 then
        failwith "not enough key-signature matches"
    | ((Some _validator_key :: vk_tl), (Some _signature :: sig_tl)) ->
        root_hash_check_signatures (vk_tl, sig_tl, block_hash, (remaining - 1))
    | ((_ :: vk_tl), (None :: sig_tl)) ->
      root_hash_check_signatures (vk_tl, sig_tl, block_hash, remaining)
    | ((None :: vk_tl), (_ :: sig_tl)) ->
      root_hash_check_signatures (vk_tl, sig_tl, block_hash, remaining)
    | (_, _) ->
      failwith "validators and signatures have different size"

let root_hash_check_keys
  (action: root_hash_action)
  (storage: root_hash_storage)
  (block_hash: blake2b) =
    let validators_length = (int (List.length storage.current_validators)) in
    let required_validators = (validators_length * 2) / 3 in
    root_hash_check_keys (
      action.current_validator_keys,
      storage.current_validators,
      block_hash,
      required_validators
    )


let root_hash_check_signatures
  (action: root_hash_action)
  (storage: root_hash_storage)
  (signatures: signatures)
  (block_hash: blake2b) =
    let validators_length = (int (List.length storage.current_validators)) in
    let required_validators = (validators_length * 2) / 3 in
    root_hash_check_signatures (
      action.current_validator_keys,
      signatures,
      block_hash,
      required_validators
    )

let root_hash_main
  (root_hash_update: root_hash_action)
  (storage: root_hash_storage) =
    let block_hash = root_hash_block_hash root_hash_update in
    let block_height = root_hash_update.block_height in
    let state_hash = root_hash_update.state_hash in
    let validators = root_hash_update.validators in
    let signatures = root_hash_update.signatures in

    let () = root_hash_check_block_height storage block_height in
    let () = root_hash_check_signatures root_hash_update storage signatures block_hash in
    let () = root_hash_check_keys root_hash_update storage block_hash in

    {
      current_block_hash = block_hash;
      current_block_height = block_height;
      current_state_hash = state_hash;
      current_validators = validators;
    }

(* main contract *)
type storage = {
  root_hash: root_hash_storage;
}
type action = root_hash_action

let main (action, storage : action * storage) =
  let { root_hash } = storage in
  let root_hash = root_hash_main action root_hash in
    (([] : operation list), { root_hash = root_hash; })
  