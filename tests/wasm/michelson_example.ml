let parse code =
  let open Tezos_micheline.Micheline_parser in
  let r, _ = tokenize code in
  let r, _ = parse_expression r in
  r
  |> Tezos_micheline.Micheline.strip_locations
  |> Tezos_micheline.Micheline.map
    (fun x ->
      x
      |> Tezos.Michelson.Michelson_v1_primitives.prim_of_string
      (* TODO: Error reporting *)
      |> Result.get_ok)

let q x =
  Wasm.Source.(x @@ no_region)

open Tezos_micheline.Micheline

type locals = { mutable current: int
              ; mutable max: int }

type context =
  { mutable imports: string list
  ; locals: locals }

let find a lst =
  let rec aux idx lst =
    match lst with
    | [] -> -1
    | hd :: _ when hd = a -> idx
    | _ :: tl -> aux (idx + 1) tl
  in
  aux 0 lst

let alloc_func ~ctx name =
  if not (List.mem name ctx.imports) then
    ctx.imports <- name :: ctx.imports;
  "$" ^ name

let alloc_local ctx =
  let local = ctx.locals.current in
  ctx.locals.current <- local + 1;
  ctx.locals.max <- Int.max ctx.locals.current ctx.locals.max;
  Int32.of_int local

let free_local ctx =
  ctx.locals.current <- ctx.locals.current - 1

type stack_value =
  | Stack
  | Const of int32
  | Local of int32
  | Memory of int32
  | Indirect of stack_value * int32

let rec get_value ctx depth v =
  match v with
  | Stack ->
    if depth = 0 then "", Stack
    else
      let local = alloc_local ctx in
      Printf.sprintf "local.set %ld" local, Local local
  | Const n -> Printf.sprintf {|
      i32.const %ld
    |} n, Stack
  | Local n -> Printf.sprintf "local.get %ld" n, Stack
  | Memory n ->
    Printf.sprintf {|
      i32.const %ld
      i32.load
    |} n, Stack
  | Indirect (v, offset) ->
    let instr, _ = get_value ctx 0 v in
    Printf.sprintf {|
      %s
      i32.load offset=%ld
    |} instr offset, Stack

let alloc ctx size =
  let fn = alloc_func ~ctx "alloc" in
  Printf.sprintf {|
    i32.const %ld
    call %s
  |} size fn

let env_import =
  [ "alloc", "(import \"env\" \"alloc\" (func $alloc (param i32) (result i32)))"
  ; "z_add", "(import \"env\" \"z_add\" (func $z_add (param i32 i32) (result i32)))"
  ; "z_sub", "(import \"env\" \"z_sub\" (func $z_sub (param i32 i32) (result i32)))" ]

let generate_module code =
  let open Tezos.Michelson.Michelson_v1_primitives in
  let ctx = { imports = []; locals = { max = 1; current = 1 } } in
  let rec step instr stack code =
    match stack, code with
    | top :: _, [] ->
      let top, _ = get_value ctx 0 top in
      instr ^ top

    | pair :: stack, Prim (_, I_UNPAIR, _, _) :: code ->
      let a, _ = get_value ctx 0 (Indirect (pair, 0l)) in
      let b, _ = get_value ctx 0 (Indirect (pair, 4l)) in
      step (instr ^ b ^ a) (Stack :: Stack :: stack) code

    | a :: b :: stack, Prim (_, I_ADD, _, _) :: code ->
      let a, _ = get_value ctx 0 a in
      let b, _ = get_value ctx 0 b in
      let fn = alloc_func ~ctx "z_add" in

      let instr =
        Printf.sprintf {|
          %s
          %s
          %s
          call %s
        |} instr b a fn
      in
      step instr (Stack :: stack) code

    | a :: b :: stack, Prim (_, I_SUB, _, _) :: code ->
        let a, _ = get_value ctx 0 a in
        let b, _ = get_value ctx 0 b in
        let fn = alloc_func ~ctx "z_sub" in
  
        let instr =
          Printf.sprintf {|
            %s
            %s
            %s
            call %s
          |} instr b a fn
        in
        step instr (Stack :: stack) code

    | stack, Prim (_, I_NIL, _, _) :: code ->
      step instr (Const 0l :: stack) code

    | a :: b :: stack, Prim (_, I_PAIR, _, _) :: code ->
      let addr = alloc ctx 8l in
      let a, _ = get_value ctx 0 a in
      let b, b_value = get_value ctx 1 b in
      let b_value, _ = get_value ctx 0 b_value in
      let local = alloc_local ctx in
      free_local ctx;
      free_local ctx; (* TODO: Properly free local variable for `b` *)

      let instr =
        Printf.sprintf {|
          %s
          ;; PAIR
          %s ;; b
          %s ;; Pair allocated address
          local.tee %ld
          %s
          i32.store offset=4
          local.get %ld
          %s ;; a
          i32.store
        |} instr b addr local b_value local a
      in
      step instr (Local local :: stack) code

    | pred :: stack, Prim (_, I_IF, [ Seq (_, consequent); Seq (_, alternative) ], _) :: code ->
      let pred, _ = get_value ctx 0 pred in
      let instr =
        Printf.sprintf {|
          %s
          %s
          (if
            (then %s)
            (else %s))
        |} instr pred (step "" stack consequent) (step "" stack alternative)
      in
      step instr stack code

    | a :: _, Prim (loc, I_IF_LEFT, params, annot) :: code ->
      let code = Prim (loc, I_IF, params, annot) :: code in
      let a, a_value = get_value ctx 0 a in
      let instr =
        Printf.sprintf {|
          %s
          %s
          i32.const 4
          i32.sub
          i32.load
          i32.const 6
          i32.eq
        |} instr a
      in
      step instr (Stack :: Indirect (a_value, 0l) :: stack) code

    | a :: b :: stack, Prim (_, I_SWAP, _, _) :: code ->
      let a, a_value = get_value ctx 0 a in
      let b, b_value = get_value ctx 1 b in
      let instr =
        Printf.sprintf {|
          %s
          %s
          %s
        |} instr a b
      in
      step instr (b_value :: a_value :: stack) code

    | a :: stack, Prim (loc, I_DROP, [ Int (int_loc, z) ], annot) :: code ->
      let instr =
        match a with
        | Stack -> instr ^ "drop\n"
        | _ -> instr
      in
      let code =
        if Z.compare z (Z.of_int 0) = 0 then code
        else Prim (loc, I_DROP, [ Int (int_loc, Z.sub z (Z.of_int 1)) ], annot) :: code
      in
      step instr stack code

    | stack, Prim (_, I_PUSH, [ _; Int (_, _) ], _) :: code ->
      step instr ((Const 0l) :: stack) code

    | _, Prim (_, prim, _, _) :: _ ->
      failwith (Printf.sprintf "Unsupported primitive in this context %s" (string_of_prim prim))
    | _ -> assert false
  in
  let instr = step "" [Local 0l] code in
  let locals =
    let rec aux = function
    | 0 -> ""
    | n -> "(local i32) " ^ aux (n - 1)
    in
    aux (ctx.locals.max - 1)
  in
  let imports =
    ctx.imports
    |> List.map (fun k -> List.assoc k env_import)
    |> String.concat "\n"
  in
  let mod_ =
    Printf.sprintf {|
      (module
        %s ;; Imports
        (memory (export "memory") 1)
        (func (export "main") (param i32) (result i32)
          %s ;; Locals
          %s))
    |} imports locals instr
  in
  Format.printf "%s\n\n" mod_;
  mod_

let compile : Tezos.Michelson.Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical -> Wasm.Ast.module_ =
  fun ast ->
    let open Tezos_micheline.Micheline in
    let open Tezos.Michelson.Michelson_v1_primitives in
    let root = root ast in
    match root with
    | Seq (_, [ Prim _; Prim _; Prim (_, K_code, [ Seq (_, code) ], _) ]) ->
      (match Wasm.Parse.string_to_module @@ generate_module code with
      | { it = Textual mod_ ; _ } -> mod_
      | _ -> assert false)
    | _ -> assert false

type value =
  | Int of Z.t
  | Left of value
  | Right of value

let malloc memory size =
  let current =
    match Wasm.Memory.load_num memory 0L 0l Wasm.Types.I32Type with
    | I32 v -> v
    | _ -> assert false
  in
  Wasm.Memory.store_num memory 0L 0l (I32 (Int32.add current size));
  current

module Context = struct
  type t = { values : (int32, Z.t) Hashtbl.t
           ; mutable count : int32
           ; mutable memory : Wasm.Memory.t }

  let create () =
    { values = Hashtbl.create 16
    ; count = 0l
    ; memory = Wasm.Memory.alloc (Wasm.Types.MemoryType { min = 0l; max = None }) }

  let rec alloc t z =
    match z with
    | Int z ->
      let s = t.count in
      Hashtbl.add t.values s z;
      t.count <- Int32.add s 1l;
      Int32.(logor (shift_left s 1) 1l)
    | Left v ->
      let address = malloc t.memory 8l in
      Wasm.Memory.store_num t.memory (Int64.of_int32 address) 0l (I32 6l);
      let value = alloc t v in
      Wasm.Memory.store_num t.memory (Int64.of_int32 address) 4l (I32 value);
      (Int32.add address 4l)
    | Right v ->
      let address = malloc t.memory 8l in
      Wasm.Memory.store_num t.memory (Int64.of_int32 address) 0l (I32 9l);
      let value = alloc t v in
      Wasm.Memory.store_num t.memory (Int64.of_int32 address) 4l (I32 value);
      (Int32.add address 4l)

  let get t n =
    match Hashtbl.find_opt t.values n with
    | Some v -> Int v
    | None -> failwith (Printf.sprintf "Zarith number not found %ld" n)

end

let make_extern_list module_ context =
  let alloc values =
    let size =
      match values with
      | [ Wasm.Values.Num (I32 size) ] -> size
      | _ -> assert false
    in
    let current = malloc context.Context.memory size in
    [ Wasm.Values.Num (I32 current) ]
  in

  let f_2op f values =
      let a, b =
      match values with
      | Wasm.Values.[ Num (I32 a); Num (I32 b) ] -> a, b
      | _ -> assert false
    in
    let a = match Context.get context a with Int x -> x | _ -> assert false in
    let b = match Context.get context b with Int x -> x | _ -> assert false in
    let result = Context.alloc context (Int (f a b)) in
    [ Wasm.Values.Num (I32 result) ]
  in

  let z_add = f_2op Z.add in
  let z_sub = f_2op Z.sub in

  let imports =
    Wasm.Instance.[
      "alloc", ExternFunc (Wasm.Func.alloc_host (FuncType ([NumType I32Type], [NumType I32Type])) alloc);
      "z_add", ExternFunc (Wasm.Func.alloc_host (FuncType ([NumType I32Type; NumType I32Type], [NumType I32Type])) z_add);
      "z_sub", ExternFunc (Wasm.Func.alloc_host (FuncType ([NumType I32Type; NumType I32Type], [NumType I32Type])) z_sub);
    ]
  in

  List.map
    (fun import ->
      let k = Wasm.Utf8.encode import.Wasm.Source.it.Wasm.Ast.item_name in
      List.assoc k imports )
    module_.Wasm.Source.it.Wasm.Ast.imports


let exec m arg =
  let context = Context.create () in
  let m = Wasm.Eval.init (ref max_int) m (make_extern_list m context) in
  let main =
    match Wasm.Instance.export m (Wasm.Utf8.decode "main") with
    | Some (ExternFunc x) -> x
    | _ -> assert false
  in
  let memory =
    match Wasm.Instance.export m (Wasm.Utf8.decode "memory") with
    | Some (ExternMemory m) -> m
    | _ -> assert false
  in
  let () =
    let argument = Context.alloc context arg in
    let storage = Context.alloc context (Int (Z.of_int 42)) in
    Wasm.Memory.store_num memory 0L 0l (I32 12l);
    Wasm.Memory.store_num memory 4L 0l (I32 argument);
    Wasm.Memory.store_num memory 8L 0l (I32 storage);
    context.memory <- memory
  in

  let () =
    let content = Wasm.Memory.load_bytes memory 0L 20 in
    Format.printf "%s\n" Bytes.(to_string (escaped (of_string content)))
  in

  match Wasm.Eval.invoke (ref max_int) main [ Wasm.Values.(Num (I32 4l)) ] with
  | [ Wasm.Values.Num (I32 address) ] ->
    (let operations = Wasm.Memory.load_num memory (Int64.of_int32 address) 0l I32Type in
    let storage = Wasm.Memory.load_num memory (Int64.of_int32 address) 4l I32Type in
    match operations, storage with
    | (I32 a, I32 b) -> (a, Context.get context b)
    | _ -> assert false)
  | _ -> assert false

let () =
  let module_ =
    (* {| { parameter int; storage int; code { UNPAIR; SUB; NIL operation; PAIR } } |} *)
    {|  { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
      storage int ;
      code { UNPAIR ;
           IF_LEFT { IF_LEFT { SWAP ; ADD } { ADD } } { DROP 2 ; PUSH int 0 } ;
           NIL operation ;
           PAIR } }
    |}
    |> parse
    |> compile
  in
  Wasm.Print.module_ stdout 0 module_;
  let a, b = exec module_ (Int (Z.of_int 10)) in
  let b = match b with Int x -> x | _ -> assert false in
  Format.printf "Operations: %ld, Storage: %a\n" a Z.pp_print b
