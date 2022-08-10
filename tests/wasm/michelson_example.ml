let retard =
  {|
{ parameter (or (int %decrement) (int %increment)) ;
storage int ;
code { DUP ;
       CDR ;
       DIP { DUP } ;
       SWAP ;
       CAR ;
       IF_LEFT
         { DUP ;
           DIP { DIP { DUP } ; SWAP } ;
           PAIR ;
           DUP ;
           CDR ;
           DIP { DUP ; CAR } ;
           SUB ;
           DIP { DROP 2 } }
         { DUP ;
           DIP { DIP { DUP } ; SWAP } ;
           PAIR ;
           DUP ;
           CDR ;
           DIP { DUP ; CAR } ;
           ADD ;
           DIP { DROP 2 } } ;
       NIL operation ;
       PAIR ;
       DIP { DROP 2 } } }
       |}

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

type context =
  { mutable imports: string list
  ; mutable locals: int }

let find a lst =
  let rec aux idx lst =
    match lst with
    | [] -> -1
    | hd :: _ when hd = a -> idx
    | _ :: tl -> aux (idx + 1) tl
  in
  aux 0 lst

let alloc_func ~ctx name =
  let idx = find name ctx.imports in
  let len = List.length ctx.imports in
  if idx < 0 then begin
    ctx.imports <- name :: ctx.imports;
    Int32.of_int len
  end else
    Int32.of_int (len - idx)

let alloc_local ctx =
  let local = ctx.locals in
  ctx.locals <- ctx.locals + 1;
  Int32.of_int local

type value =
  | Stack
  | Const of int32
  | Local of int32
  | Memory of int32
  | Indirect of value * int32

let rec get_value ctx depth v =
  let open Wasm.Operators in
  match v with
  | Stack ->
    if depth = 0 then [], Stack
    else
      let local = alloc_local ctx in
      [ local_set (q local) ], Local local
  | Const n -> [ i32_const (q n) ], Stack
  | Local n -> [ local_get (q n) ], Stack
  | Memory n -> [ i32_const (q n); i32_load 2 0l ], Stack
  | Indirect (v, offset) ->
    let instr, _ = get_value ctx 0 v in
    (instr @ [ i32_load 2 offset ]), Stack

let alloc ctx size =
  let fn = alloc_func ~ctx "alloc" in
  let open Wasm.Operators in
  [ i32_const (q size)
  ; call (q fn) ]

let env_import =
  let open Wasm.Types in
  [ "alloc", FuncType ([ NumType I32Type ], [ NumType I32Type ])
  ; "z_add", FuncType ([ NumType I32Type; NumType I32Type ], [ NumType I32Type ]) ]

let generate_module code =
  let open Tezos.Michelson.Michelson_v1_primitives in
  let ctx = { imports = []; locals = 1 } in
  let rec step instr stack code =
    match stack, code with
    | top :: _, [] ->
      let top, _ = get_value ctx 0 top in
      top @ instr
    | pair :: stack, Prim (_, I_UNPAIR, _, _) :: code ->
      let a, _ = get_value ctx 0 (Indirect (pair, 0l)) in
      let b, _ = get_value ctx 0 (Indirect (pair, 4l)) in
      step (List.rev a @ List.rev b @ instr) (Stack :: Stack :: stack) code
    | a :: b :: stack, Prim (_, I_ADD, _, _) :: code ->
      let a, _ = get_value ctx 0 a in
      let b, _ = get_value ctx 0 b in
      let fn = alloc_func ~ctx "z_add" in
      step (Wasm.Operators.call (q fn) :: List.rev a @ List.rev b @ instr) (Stack :: stack) code
    | stack, Prim (_, I_NIL, _, _) :: code ->
      step instr (Const 0l :: stack) code
    | a :: b :: stack, Prim (_, I_PAIR, _, _) :: code ->
      let addr = List.rev @@ alloc ctx 8l in
      let a, _ = get_value ctx 0 a in
      let b, b_value = get_value ctx 1 b in
      let b_value, _ = get_value ctx 0 b_value in
      let local = alloc_local ctx in
      step Wasm.Operators.(
        i32_store 2 4l :: List.rev a @ local_get (q local)
        :: i32_store 2 0l :: List.rev b_value @ local_tee (q local)
        :: addr
        @ List.rev b
        @ instr)
        (Local local :: stack)
        code
    | _ -> assert false
  in
  let instr = step [] [Local 0l] code in
  let locals =
    let rec aux = function
    | 0 -> []
    | n -> Wasm.Types.NumType I32Type :: aux (n - 1)
    in
    aux (ctx.locals - 1)
  in
  let main =
    Wasm.Ast.{ ftype = q 0l
             ; locals = locals
             ; body = List.rev_map q instr }
  in
  let types =
    Wasm.Types.(FuncType ([NumType I32Type], [NumType I32Type]))
    :: List.map (fun name -> List.assoc name env_import) ctx.imports
  in
  let imports =
    let import types name =
      let type_ = List.assoc name env_import in
      let idx = Int32.of_int @@ find type_ types in
      q Wasm.Ast.{ module_name = Wasm.Utf8.decode "env"
                 ; item_name = Wasm.Utf8.decode name
                 ; idesc = q (FuncImport (q idx))}
    in
    List.rev_map (import types) ctx.imports
  in
  Wasm.Ast.{ empty_module with
             types = List.map q types
           ; imports
           ; memories = [ q { mtype = Wasm.Types.MemoryType { min = 1l; max = Some(1l) } } ]
           ; funcs = [ q main ]
           ; exports = [ q { name = Wasm.Utf8.decode "main"
                           ; edesc = q (FuncExport (imports |> List.length |> Int32.of_int |> q)) }
                       ; q { name = Wasm.Utf8.decode "memory"
                           ; edesc = q (MemoryExport (q 0l)) }] }

let compile : Tezos.Michelson.Michelson_v1_primitives.prim Tezos_micheline.Micheline.canonical -> Wasm.Ast.module_ =
  fun ast ->
    let open Tezos_micheline.Micheline in
    let open Tezos.Michelson.Michelson_v1_primitives in
    let root = root ast in
    match root with
    | Seq (_, [ Prim _; Prim _; Prim (_, K_code, [ Seq (_, code) ], _) ]) ->
      q (generate_module code)
    | _ -> assert false

let () =
  {| { parameter int; storage int; code { UNPAIR; ADD; NIL operation; PAIR } } |}
  |> parse
  |> compile
  |> Wasm.Print.module_ stdout 0