
module Parse: sig
  type t = (int, Tezos.Michelson.Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node list
  val parse : string -> t
end = struct
  type t = (int, Tezos.Michelson.Michelson_v1_primitives.prim) Tezos_micheline.Micheline.node list

  open Tezos_micheline.Micheline
  open Tezos_micheline.Micheline_parser

  let prim_of_string prim =
    match Tezos.Michelson.Michelson_v1_primitives.prim_of_string prim with
    | Ok prim -> prim
    | Error _ -> failwith ("Invalid primitive " ^ prim)

  let parse code =
    (* TODO: It'd be good to do typechecking and use Tezos' Script_typed_ir instead, check Script_ir_translator *)
    let ast =
      let tokens, _ = tokenize code in
      let ast, _ = parse_toplevel tokens in
      List.map
        (fun node -> root @@ map prim_of_string @@ strip_locations node)
        ast
    in
    ast
end

type union = Left of Wasm.Values.ref_ | Right of Wasm.Values.ref_

type Wasm.Values.ref_ += Int of Z.t
type Wasm.Values.ref_ += String of string
type Wasm.Values.ref_ += Bool of bool
type Wasm.Values.ref_ += Pair of Wasm.Values.ref_ * Wasm.Values.ref_
type Wasm.Values.ref_ += Union of union
type Wasm.Values.ref_ += List of Wasm.Values.ref_ list
type Wasm.Values.ref_ += Option of Wasm.Values.ref_ option
type Wasm.Values.ref_ += Unit

module Values = struct
  type t = Wasm.Values.ref_

  let compare x y =
    match x, y with
    | String a, String b -> String.compare a b
    | _ -> assert false
end

module ValueMap = Map.Make(Values)
module ValueSet = Set.Make(Values)

type Wasm.Values.ref_ += Map of Wasm.Values.ref_ ValueMap.t
type Wasm.Values.ref_ += Set of ValueSet.t

let () = Wasm.Values.type_of_ref' := (function _ -> ExternRefType)

let pp_list f fmt lst =
  let open Format in
  pp_print_char fmt '[';
  let rec aux lst =
    match lst with
    | [] -> ()
    | [ item ] -> fprintf fmt "%a" f item
    | item :: lst -> fprintf fmt "%a, " f item; aux lst
  in
  aux lst;
  pp_print_char fmt ']'

let rec pp_ref fmt = function
  | Int z -> Z.pp_print fmt z
  | String s -> Format.fprintf fmt "String \"%s\"" (String.escaped s)
  | Bool b -> Format.fprintf fmt (if b then "true" else "false")
  | Pair (a, b) -> Format.fprintf fmt "(%a, %a)" pp_ref a pp_ref b
  | Union (Left ref) -> Format.fprintf fmt "Left %a" pp_ref ref
  | Union (Right ref) -> Format.fprintf fmt "Right %a" pp_ref ref
  | List lst -> pp_list pp_ref fmt lst
  | Option (Some ref) -> Format.fprintf fmt "Some %a" pp_ref ref
  | Option None -> Format.fprintf fmt "None"
  | Unit -> Format.pp_print_string fmt "Unit";
  | Wasm.Values.NullRef _ -> Format.fprintf fmt "null"
  | _ -> assert false

module CoreLib = struct

  let compare x y =
    Int (Z.of_int (Values.compare x y))

  open Wasm

  let car pair =
    match pair with
    | Pair (fst, _) -> fst
    | _ -> assert false

  let cdr pair =
    match pair with
    | Pair (_, snd) -> snd
    | _ -> assert false

  let pair snd fst =
    Pair (fst, snd)

  let unpair = function
  | Pair (fst, snd) -> (fst, snd)
  | _ -> assert false

  let z_add x y =
    match x, y with
    | Int x, Int y -> Int (Z.add x y)
    | _ -> assert false

  let z_sub y x =
    match  x,y  with
    | Int x, Int y -> Int (Z.sub x y)
    | _ -> assert false

  let is_left v =
    match v with
    | Union (Left v) -> (v, 1l)
    | Union (Right v) -> (v, 0l)
    | _ -> assert false

  let iter v =
    match v with
    | List (item :: []) -> item, List [], 0l
    | List (item :: rest) -> item, List rest, 1l
    | _ -> assert false

  let deref_bool v =
    match v with
    | Bool true -> 1l
    | Bool false -> 0l
    | _ -> assert false

  let failwith msg =
    match msg with
    | String msg -> failwith msg
    | _ -> assert false

  let rec get_n n p =
    match n, p with
    | 0l, p -> p
    | 1l, Pair (fst, _) -> fst
    | 2l, Pair (_, snd) -> snd
    | n, Pair (_, snd) -> get_n Int32.(sub n 2l) snd
    | _ -> assert false

  let is_none opt =
    match opt with
    | Option (Some v) -> (v, 0l)
    | Option None -> (Int Z.zero, 1l)
    | _ -> assert false

  let isnat i =
    match i with
    | Int z ->
      if Z.compare z Z.zero >= 0 then Option (Some i)
      else Option None
    | _ -> assert false

  let mem k m =
    match m with
    | Map m -> Bool (ValueMap.mem k m)
    | Set s -> Bool (ValueSet.mem k s)
    | _ -> assert false

  let map_get k m =
    match m with
    | Map m -> Option (ValueMap.find_opt k m)
    | p ->
      Format.printf "%a\n" pp_ref p;
      assert false

  let neq x =
    let x =
      match x with
      | Int z -> Z.to_int z
      | _ -> assert false
    in
    match x with
    | 0 -> Bool false
    | _ -> Bool true

  let not b =
    match b with
    | Bool b -> Bool (not b)
    | _ -> assert false

  let or_ p q =
    match p, q with
    | Bool p, Bool q -> Bool (p || q)
    | _ -> assert false

  let some v =
    Option (Some v)

  let string_mock _ =
    String "MOCKED"

  let update k v map =
    match map, v with
    | Map map, Option v -> Map (ValueMap.update k (fun _ -> v) map)
    | _ -> assert false

  let func type_ f =
    Instance.ExternFunc (Func.alloc_host type_ f)

  let ref__ref f =
    func Types.(FuncType ([RefType ExternRefType], [RefType ExternRefType]))
      Values.(function
      | [ Ref a ] -> [ Ref (f a) ]
      | _ -> assert false)

  let ref_ref__ref f =
    func Types.(FuncType ([RefType ExternRefType; RefType ExternRefType], [RefType ExternRefType]))
      Values.(function
      | [ Ref a; Ref b ] -> [ Ref (f a b) ]
      | _ -> assert false)

  let ref_ref_ref__ref f =
    func Types.(FuncType ([RefType ExternRefType; RefType ExternRefType; RefType ExternRefType], [RefType ExternRefType]))
      Values.(function
      | [ Ref a; Ref b; Ref c ] -> [ Ref (f a b c) ]
      | _ -> assert false)

  let ref__ref_ref f =
    func Types.(FuncType ([RefType ExternRefType], [ RefType ExternRefType; RefType ExternRefType]))
      Values.(function
      | [ Ref a ] -> let (a, b) = f a in [ Ref b; Ref a ]
      | _ -> assert false)

  let ref__ref_i32 f =
    func Types.(FuncType ([RefType ExternRefType], [ RefType ExternRefType; NumType I32Type ]))
      Values.(function
      | [ Ref a ] -> let (a, b) = f a in [ Ref a; Num (I32 b) ]
      | _ -> assert false)

  let ref__ref_ref_i32 f =
    func Types.(FuncType ([RefType ExternRefType], [ RefType ExternRefType; RefType ExternRefType; NumType I32Type ]))
      Values.(function
      | [ Ref a ] -> let (a, b, c) = f a in [ Ref a; Ref b; Num (I32 c) ]
      | _ -> assert false)

  let const value =
    func Types.(FuncType ([], [RefType ExternRefType]))
      Values.(function
      | [] -> [ Ref value ]
      | _ -> assert false)

  let ref__i32 f =
    func Types.(FuncType ([RefType ExternRefType], [NumType I32Type]))
      Values.(function
      | [ Ref b ] -> [ Num (I32 (f b)) ]
      | _ -> assert false)

  let ref__ f =
    func Types.(FuncType ([RefType ExternRefType], []))
      Values.(function
      | [ Ref a ] -> f a; []
      | _ -> assert false)

  let i32_ref__ref f =
    func Types.(FuncType ([NumType I32Type; RefType ExternRefType], [RefType ExternRefType]))
      Values.(function
      | [ Num (I32 n); Ref a ] -> [ Ref (f n a) ]
      (* TODO: remove *)
      | Ref x :: Num (I32 _) :: _ -> Format.printf "ERROR: %a\n" pp_ref x; Stdlib.failwith "..."
      | _ -> assert false)

  let i32__ref f =
    func Types.(FuncType ([NumType I32Type], [RefType ExternRefType]))
      Values.(function
      | [ Num (I32 n) ] -> [ Ref (f n) ]
      | _ -> assert false)

  let exports =
    [ "car", ref__ref car
    ; "cdr", ref__ref cdr
    ; "compare", ref_ref__ref compare
    ; "pair", ref_ref__ref pair
    ; "unpair", ref__ref_ref unpair
    ; "z_add", ref_ref__ref z_add
    ; "z_sub", ref_ref__ref z_sub
    ; "nil", const (List [])
    ; "zero", const (Int Z.zero)
    ; "empty_set", const (Set ValueSet.empty)
    ; "is_left", ref__ref_i32 is_left
    ; "is_none", ref__ref_i32 is_none
    ; "isnat", ref__ref isnat
    ; "iter", ref__ref_ref_i32 iter
    ; "deref_bool", ref__i32 deref_bool
    ; "failwith", ref__ failwith
    ; "get_n", i32_ref__ref get_n
    ; "map_get", ref_ref__ref map_get
    ; "mem", ref_ref__ref mem
    ; "neq", ref__ref neq
    ; "not", ref__ref not
    ; "or", ref_ref__ref or_
    ; "sender", const (String "tz1gq9WKoVEiq69FgTrCkxDQfdJmevgHWKA7") (* MOCKED FOR NOW *)
    ; "some", ref__ref some
    ; "string", i32__ref string_mock
    ; "update", ref_ref_ref__ref update ]

  let signatures =
    let ref_ref__ref = "(param externref externref) (result externref)" in
    let ref_ref_ref__ref = "(param externref externref externref) (result externref)" in
    let ref__ref_ref = "(param externref) (result externref externref)" in
    let ref__ref = "(param externref) (result externref)" in
    let ref__ref_i32 = "(param externref) (result externref i32)" in
    let ref__ref_ref_i32 = "(param externref) (result externref externref i32)" in
    let ref__i32 = "(param externref) (result i32)" in
    let i32__ref = "(param i32) (result externref)" in
    let i32_ref__ref = "(param i32 externref) (result externref)" in
    let ref__ = "(param externref)" in
    let const = "(result externref)" in
    let func type_ name = name, Printf.sprintf "(import \"env\" \"%s\" (func $%s %s))" name name type_ in
    [ func ref_ref__ref "pair"
    ; func ref__ref_ref "unpair"
    ; func ref_ref__ref "z_add"
    ; func ref_ref__ref "z_sub"
    ; func ref_ref__ref "compare"
    ; func ref__ref "car"
    ; func ref__ref "cdr"
    ; func ref__ref "some"
    ; func const "nil"
    ; func const "zero"
    ; func const "empty_set"
    ; func const "sender"
    ; func ref_ref__ref "map_get"
    ; func ref_ref__ref "mem"
    ; func ref_ref_ref__ref "update"
    ; func ref__ref_ref_i32 "iter"
    ; func ref__ref_i32 "is_left"
    ; func ref__ref_i32 "is_none"
    ; func ref__ref "isnat"
    ; func ref__ref "not"
    ; func ref_ref__ref "or"
    ; func ref__i32 "deref_bool"
    ; func ref__ref "neq"
    ; func i32__ref "string"
    ; func ref__ "failwith"
    ; func i32_ref__ref "get_n" ]

  let link mod_ =
    let find_export name =
      let name = Utf8.encode name in
      match List.assoc_opt name exports with
      | Some f -> f
      | None -> Stdlib.failwith ("Export not found " ^ name)
    in
    List.map
      (fun Source.{ it = Ast.{ item_name; _ }; _ } ->
        find_export item_name)
      mod_.Source.it.Ast.imports
end

let rec drop n lst =
  match n, lst with
  | 0, lst -> lst
  | n, _ :: lst -> drop (n - 1) lst
  | _, [] -> raise (Invalid_argument "drop")

let take n lst =
  let rec aux n head tail =
    match n, tail with
    | 0, _ -> head
    | n, item :: tail -> aux (n - 1) (item :: head) tail
    | _, [] -> raise (Invalid_argument "take")
  in
  List.rev @@ aux n [] lst

module Compiler: sig
  type t = Wasm.Ast.module_

  val compile : Parse.t -> t
end = struct
  type t = Wasm.Ast.module_

  open Tezos_micheline.Micheline
  open Tezos.Michelson.Michelson_v1_primitives

  module StringSet = Set.Make(String)

  type code = string

  type context = { mutable imports: StringSet.t
                 ; mutable functions: code list
                 ; mutable locals: string list
                 ; mutable strings: string }

  let import context name =
    if not (StringSet.mem name context.imports) then
      context.imports <- StringSet.add name context.imports

  let alloc_lambda context code =
    let index = List.length context.functions in
    context.functions <- code :: context.functions;
    index

  let alloc_local context type_ =
    let local = List.length context.locals in
    context.locals <- type_ :: context.locals;
    Int32.of_int (local + 1)

  let alloc_string context str =
    let address = String.length context.strings in
    context.strings <- context.strings ^ str;
    address

  type stack =
    | Placed
    | Lambda of int
    | Apply of stack * int
    | Local of int32

  let s = Printf.sprintf

  let rec  emit_stack_value value =
    match value with
    | Placed -> ""
    | Lambda l -> s "call $%d" l
    | Apply (l, n) -> s "%s call $pair call $%d" (emit_stack_value l) n
    | Local n -> s "local.get %ld" n

  let rec pp_stack_value fmt stack =
    let open Format in
    match stack with
    | Placed -> pp_print_string fmt "Placed"
    | Lambda l -> fprintf fmt "Lambda %d" l
    | Apply (l, n) -> fprintf fmt "Apply (%a, %d)" pp_stack_value l n
    | Local n -> fprintf fmt "Local %ld" n

  let pp_stack = pp_list pp_stack_value

  let save_stack context value =
    match value with
    | Placed ->
      let local = alloc_local context "externref" in
      s "local.set %ld" local, Local local
    | _ -> "", value

  let dig context stack =
    let rec aux saved stack code =
      match stack with
      | top :: stack ->
        let save_code, value = save_stack context top in
        aux (value :: saved) stack (s "%s %s" code save_code)
      | [] -> List.rev saved, code
      in
      aux [] stack ""

  let rec step context stack instr code =
    let () =
    match instr with Prim (_, prim, _, _) :: _ ->
      Format.printf "%s Stack: %a Size: %d\n"
        (Tezos.Michelson.Michelson_v1_primitives.string_of_prim prim)
        pp_stack stack
        (List.length stack)
    | _ -> ()
    in
    match stack, instr with
    | stack, [] ->
      code, stack

    | pair :: stack, Prim (_, I_UNPAIR, _, _) :: instr ->
      import context "unpair";
      step context (Placed :: Placed :: stack) instr
        (s "%s \n;; UNPAIR\n%s call $unpair" code (emit_stack_value pair))

    | pair :: stack, Prim (_, I_CAR, _, _) :: instr ->
      import context "car";
      step context (Placed :: stack) instr
        (s "%s \n;; CAR\n%s call $car" code (emit_stack_value pair))

    | pair :: stack, Prim (_, I_CDR, _, _) :: instr ->
      import context "cdr";
      step context (Placed :: stack) instr
        (s "%s \n;; CDR\n%s call $cdr" code (emit_stack_value pair))

    | x :: y :: stack, Prim (_, I_ADD, _, _) :: instr ->
      import context "z_add";
      step context (Placed :: stack) instr
        (s "%s\n ;; ADD\n %s %s call $z_add" code (emit_stack_value x) (emit_stack_value y))

    | x :: y :: stack, Prim (_, I_SUB, _, _) :: instr ->
      import context "z_sub";
      step context (Placed :: stack) instr
        (s "%s\n;; SUB\n %s %s call $z_sub" code (emit_stack_value x) (emit_stack_value y))

    | stack, Prim (_, I_NIL, _, _) :: instr ->
      import context "nil";
      step context (Placed :: stack) instr (s "%s\n ;; NIL\n call $nil" code)

    | fst :: snd :: stack, Prim (_, I_PAIR, _, _) :: instr ->
      import context "pair";
      step context (Placed :: stack) instr
        (s "%s\n;; PAIR \n %s %s call $pair" code (emit_stack_value fst) (emit_stack_value snd))

    | pred :: stack, Prim (_, I_IF, [ Seq (_, branch_if_left); Seq (_, branch_if_right) ], _) :: instr ->
      import context "deref_bool";
      let branch_left, _ = step context (Placed :: stack) branch_if_left "" in
      let branch_right, _ = step context (Placed :: stack) branch_if_right "" in
      (* TODO: Infer the type of the block *)
      step context stack instr
        (s {|
          %s
          ;; IF
          %s
          call $deref_bool
          (if (param externref externref)
            (then %s)
            (else %s))
        |} code (emit_stack_value pred) branch_left branch_right)

    | pred :: stack, Prim (_, I_IF_LEFT, [ Seq (_, branch_if_left); Seq (_, branch_if_right) ], _) :: instr ->
      import context "is_left";
      let branch_left, _ = step context (Placed :: stack) branch_if_left "" in
      let branch_right, _ = step context (Placed :: stack) branch_if_right "" in
      (* TODO: Infer the type of the block *)
      step context stack instr
        (s {|
          %s
          ;; IF_LEFT
          %s
          call $is_left
          (if (param externref externref)
            (then %s)
            (else %s))
        |} code (emit_stack_value pred) branch_left branch_right)

    | pred :: stack, Prim (_, I_IF_NONE, [ Seq (_, branch_if_none); Seq (_, branch_if_some) ], _) :: instr ->
      import context "is_none";
      let branch_none, _ = step context stack branch_if_none "" in
      let branch_some, _ = step context (Placed :: stack) branch_if_some "" in
      step context (Placed :: stack) instr
        (s {|
          %s
          ;; IF_NONE
          %s
          call $is_none
          (if (param externref externref)
            (then %s)
            (else %s))
        |} code (emit_stack_value pred) branch_none branch_some)

    | iterable :: acc :: stack, Prim (_, I_ITER, [ Seq (_, iter_code) ], _) :: instr ->
      import context "iter";
      let iter_code, iter_stack = step context (Placed :: Placed :: stack) iter_code "" in
      let local = alloc_local context "i32" in
      let local_iterable = alloc_local context "externref" in
      let code =
        s {|
          %s
          ;; ITER
          %s
          local.set %ld
          %s
          (loop $loop (param externref)
            local.get %ld
            call $iter
            local.set %ld
            local.set %ld

            %s

            local.get %ld
            br_if $loop)
        |} code (emit_stack_value iterable) local_iterable (emit_stack_value acc) local_iterable local local_iterable iter_code local
      in
      step context iter_stack instr code

    | fst :: snd :: stack, Prim (_, I_SWAP, _, _) :: instr ->
      let swap, stack =
        match fst, snd with
        | Placed, Placed -> "call $swap", Placed :: Placed :: stack
        | _ -> "", snd :: fst :: stack
      in
      step context stack instr
        (s "%s\n ;;SWAP\n %s" code swap)

    | stack, Prim (_, I_DROP, [], _) :: instr ->
          let drop, stack =
          match stack with
          | Placed :: stack -> "drop", stack
          | _ :: stack | stack -> "", stack
        in
        step context stack instr (s "%s\n ;; DROP\n %s" code drop)

    | stack, Prim (pl, I_DROP, [ Int (pi, z) ], a) :: instr ->
      if Z.to_int z = 0 then
        step context stack instr code
      else
        step context stack
          (Prim (pl, I_DROP, [], a) :: Prim (pl, I_DROP, [ Int (pi, Z.sub z Z.one) ], a) :: instr)
          code

    | stack, Prim (_, I_PUSH, [ _; Int (_, _) ], _) :: instr ->
      import context "zero";
      step context (Placed :: stack) instr (s "%s\n ;; PUSH 0 \n call $zero" code)

    | stack, Prim (_, I_PUSH, [ _; String (_, str) ], _) :: instr ->
      import context "string";
      let address = alloc_string context str in
      let code =
        s {|
          %s
          ;; PUSH string
          i32.const %d ;; "%s"
          call $string
        |} code address str
      in
      step context (Placed :: stack) instr code

    | message :: stack, Prim (_, I_FAILWITH, [], _) :: instr ->
      import context "failwith";
      step context stack instr (s "%s\n ;; FAILWITH\n %s call $failwith" code (emit_stack_value message))

    | stack, Prim (_, I_DUP, [ Int (_, n) ], _) :: instr ->
      let n = Z.to_int n in
      let top_stack, save_code = dig context (take n stack) in
      let bottom_stack = drop n stack in
      let dup_item = List.nth top_stack (n - 1) in
      step context (dup_item :: top_stack @ bottom_stack) instr (s "%s\n;; DUP\n %s" code save_code)

    | top :: stack, Prim (_, I_DUP, [], _) :: instr ->
      let local = alloc_local context "externref" in
      let dup_code =
        match top with
        | Placed -> s "local.set %ld" local
        | _ -> ""
      in
      step context (Local local :: Local local :: stack) instr (s "%s\n;; DUP\n %s" code dup_code)

    | stack, Prim (_, I_DIG, [ Int (_, n) ], _) :: instr ->
      let n = Z.to_int n in
      let digged, digging_code = dig context @@ take n stack in
      let save_code, saved, tail =
        match drop n stack with
        | top :: tail ->
          let save_code, value = save_stack context top in
          save_code, value, tail
        | _ -> assert false
      in
      step context (saved :: digged @ tail) instr (s "%s\n ;; DIG\n %s %s" code digging_code save_code)

    | top :: stack, Prim (_, I_DUG, [ Int (_, n) ], _) :: instr ->
      let save_code, value = save_stack context top in
      let n = Z.to_int n in
      let new_stack = take n stack @ value :: drop n stack in
      step context new_stack instr (s "%s\n ;; DUG \n %s" code save_code)

    | stack, Prim (_, I_EMPTY_SET, _, _) :: instr ->
      import context "empty_set";
      step context (Placed :: stack) instr (s "%s\n ;; EMPTY_SET\n call $empty_set" code)

    | stack, Prim (_, I_LAMBDA, [ _ ; _ ; Seq (_, lambda) ], _) :: instr ->
      let lambda_code, lambda_stack = step context [Local 0l] lambda "" in
      let lambda_code =
        match lambda_stack with
        | ret :: _ -> s "%s\n ;; LAMBDA\n %s" lambda_code (emit_stack_value ret)
        | _ -> assert false
      in
      let lambda_index = alloc_lambda context lambda_code in
      step context (Lambda lambda_index :: stack) instr code

    | top :: lambda :: stack, Prim (_, I_APPLY, [], _) :: instr ->
      let save_code, value = save_stack context top in
      let lambda = match lambda with Lambda l -> l | _ -> assert false in
      let code =
        s {|
          %s
          ;; APPLY
          %s
        |} code save_code
      in
      step context (Apply (value, lambda) :: stack) instr code

    | param :: lambda :: stack, Prim (_, I_EXEC, [], _) :: instr ->
      let call_code =
        match lambda with
        | Lambda l ->
          s "%s\n;; EXEC\n call $%d" (emit_stack_value param) l
        | Apply (param1, lambda) ->
          import context "pair";
          s "%s\n;; EXEC \n %s call $pair call $%d" (emit_stack_value param) (emit_stack_value param1) lambda
        | _ -> assert false
      in
      step context (Placed :: stack) instr (s "%s %s" code call_code)

    | key :: map :: stack, Prim (_, I_GET, [], _) :: instr ->
      import context "map_get";
      let code =
        s {|
          %s
          ;; GET
          %s
          %s
          call $map_get
        |} code (emit_stack_value key) (emit_stack_value map)
      in
      step context (Placed :: stack) instr code

    | value :: stack, Prim (_, I_GET, [ Int (_, n) ], _) :: instr ->
      import context "get_n";
      let code =
        s {|
          %s
          ;; GET
          i32.const %d
          %s
          call $get_n
        |} code (Z.to_int n) (emit_stack_value value)
      in
      step context (Placed :: stack) instr code

    | value :: stack, Prim (_, I_ISNAT, [], _) :: instr ->
      import context "isnat";
      step context (Placed :: stack) instr (s "%s\n;; ISNAT\n %s call $isnat" code (emit_stack_value value))

    | a :: b :: stack, Prim (_, I_COMPARE, [], _) :: instr ->
      import context "compare";
      step context (Placed :: stack) instr (s "%s\n ;; COMPARE\n %s %s call $compare" code (emit_stack_value a) (emit_stack_value b))

    | cmp :: stack, Prim (_, I_NEQ, [], _) :: instr ->
      import context "neq";
      step context (Placed :: stack) instr (s "%s\n;; NEQ\n %s call $neq" code (emit_stack_value cmp))

    | bool :: stack, Prim (_, I_NOT, [], _) :: instr ->
      import context "not";
      step context (Placed :: stack) instr (s "%s\n;; NOT\n %s call $not" code (emit_stack_value bool))

    | value :: stack, Prim (_, I_SOME, [], _) :: instr ->
      import context "some";
      step context (Placed :: stack) instr (s "%s\n;; SOME\n %s call $some" code (emit_stack_value value))

    | p :: q :: stack, Prim (_, I_OR, [], _) :: instr ->
      import context "or";
      step context (Placed :: stack) instr (s "%s\n;; OR\n %s %s call $or" code (emit_stack_value p) (emit_stack_value q))

    | stack, Prim (_, I_SENDER, [], _) :: instr ->
      import context "sender";
      step context (Placed :: stack) instr (s "%s\n;; SENDER\n call $sender" code)

    | k :: col :: stack, Prim (_, I_MEM, [], _) :: instr ->
      import context "mem";
      step context (Placed :: stack) instr (s "%s\n;; MEM\n %s %s call $mem" code (emit_stack_value col) (emit_stack_value k))

    | k :: v :: col :: stack, Prim (_, I_UPDATE, [], _) :: instr ->
      import context "update";
      step context (Placed :: stack) instr (s "%s\n;; UPDATE\n %s %s %s call $update" code (emit_stack_value col) (emit_stack_value v) (emit_stack_value k))

    | stack, Prim (_, prim, args, _) :: _->
      failwith
        (Printf.sprintf "Invalid instruction: %s Arguments: %d Stack size: %d\n"
          (Tezos.Michelson.Michelson_v1_primitives.string_of_prim prim)
          (List.length args)
          (List.length stack))

    | _ -> assert false

  let make_imports context =
    let find_function name =
      match List.assoc_opt name CoreLib.signatures with
      | Some f -> f
      | None -> failwith ("Function not found " ^ name)
    in
    context.imports
    |> StringSet.map find_function
    |> StringSet.to_seq
    |> List.of_seq
    |> String.concat "\n"

  let make_locals context =
    match context.locals with
    | [] -> ""
    | locals -> s "(local %s)" (String.concat " " locals)

  let make_functions context =
    context.functions
    |> List.mapi (fun idx code -> s "(func $%d (param externref) (result externref) %s)" idx code)
    |> String.concat "\n"

  let compile ast =
    let context = { imports = StringSet.empty; functions = []; locals = []; strings = "" } in
    let code =
      match ast with
      | [ Prim (_, K_parameter, _, _) ; Prim (_, K_storage, _, _); Prim (_, K_code, [ Seq (_, code) ], _) ] ->
        let body, stack = step context [Local 0l] code "" in
        let ret =
          Format.printf "-> %a\n" pp_stack stack;
          match stack with
          | ret :: _ -> emit_stack_value ret
          | [] -> ""
          (* | _ -> failwith "main function returned invalid stack" *)
        in
        s {|
          (module
            %s
            %s
            (func $swap (param externref externref) (result externref externref) local.get 1 local.get 0)
            (func (export "main") (param externref) (result externref) %s %s %s))
        |} (make_imports context) (make_functions context) (make_locals context) body ret
      | _ -> failwith "Invalid contract"
    in
    Format.printf "%s\n" code;
    match Wasm.Parse.string_to_module code with
    | { it = Textual m; _ } -> m
    | _ -> assert false
end

module Runtime: sig
  val run : Wasm.Values.ref_ -> Compiler.t -> Wasm.Values.ref_
end = struct
  open Wasm

  let run arg module_ =
    let inst = Eval.init (ref max_int) module_ (CoreLib.link module_) in
    let main =
      match Instance.export inst (Utf8.decode "main") with
      | Some (Instance.ExternFunc func) -> func
      | _ -> failwith "Missing main function"
    in
    match Eval.invoke (ref max_int) main [ Values.Ref arg ] with
    | [ Values.Ref v ] -> v
    | exception Eval.Crash (at, message) ->
      failwith (Printf.sprintf "%d %d - %d %d : %s" at.left.line at.left.column at.right.line at.right.column message)
    | _ -> failwith "Invalid return type"
end

let () =
  let value =
    {| { parameter int; storage int; code { UNPAIR; ADD; NIL operation; PAIR } } |}
    |> Parse.parse
    |> Compiler.compile
    |> Runtime.run (Pair (Int (Z.of_int 42), Int (Z.of_int 10)))
  in
  assert (Pair (List [], Int (Z.of_int 52)) = value)

let () =
  let module_ =
    {| { parameter (or (or (int %decrement) (int %increment)) (unit %reset)) ;
    storage int ;
    code { UNPAIR ;
           IF_LEFT { IF_LEFT { SWAP ; SUB } { ADD } } { DROP 2 ; PUSH int 0 } ;
           NIL operation ;
           PAIR } } |}
    |> Parse.parse
    |> Compiler.compile
  in

  let value = Runtime.run (Pair ((Union (Left (Union (Left (Int Z.one))))), Int (Z.of_int 42))) module_ in
  assert (Pair (List [], Int (Z.of_int 41)) = value);

  let value = Runtime.run (Pair ((Union (Left (Union (Right (Int Z.one))))), Int (Z.of_int 42))) module_ in
  assert (Pair (List [], Int (Z.of_int 43)) = value);

  let value = Runtime.run (Pair ((Union (Right Unit)), Int (Z.of_int 42))) module_ in
  assert (Pair (List [], Int Z.zero) = value)

let () =
  let module_ =
    {| { parameter (or (int %decrement) (int %increment));
    storage int ;
    code { UNPAIR ;
           IF_LEFT { SWAP ; SUB } { ADD };
           NIL operation ;
           PAIR } } |}
    |> Parse.parse
    |> Compiler.compile
  in

  let value = Runtime.run (Pair ((Union (Right (Int Z.one))), Int (Z.of_int 42))) module_ in
  assert (Pair (List [], Int (Z.of_int 43)) = value);

  let value = Runtime.run (Pair ((Union (Left (Int Z.one))), Int (Z.of_int 42))) module_ in
  assert (Pair (List [], Int (Z.of_int 41)) = value)

(* let () =
  let module_ =
    {| { parameter (list int) ;
         storage int ;
         code { CAR ; PUSH int 0 ; SWAP ; ITER { ADD } ; NIL ; PAIR } }
    |}
    |> Parse.parse
    |> Compiler.compile
  in

  let value = Runtime.run (Pair (List [ Int (Z.of_int 1); Int (Z.of_int 2); Int (Z.of_int 3); Int (Z.of_int 4 ) ], Int Z.zero)) module_ in
  assert (Pair (List [], Int (Z.of_int 10)) = value) *)

let () =
  let module_ =
    {| { parameter
    (or (or (pair %balance_of
               (list %requests (pair (address %owner) (nat %token_id)))
               (contract %callback
                  (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance)))))
            (list %transfer
               (pair (address %from_) (list %txs (pair (address %to_) (nat %token_id) (nat %amount))))))
        (list %update_operators
           (or (pair %add_operator (address %owner) (address %operator) (nat %token_id))
               (pair %remove_operator (address %owner) (address %operator) (nat %token_id))))) ;
  storage (map address (pair (nat %balance) (set %operators address))) ;
  code { EMPTY_SET address ;
         PUSH nat 0 ;
         PAIR ;
         LAMBDA
           (pair (pair nat (set address)) (pair address (map address (pair nat (set address)))))
           (pair nat (set address))
           { UNPAIR ; SWAP ; UNPAIR ; GET ; IF_NONE {} { SWAP ; DROP } } ;
         DUP 2 ;
         APPLY ;
         DIG 2 ;
         UNPAIR ;
         IF_LEFT
           { IF_LEFT
               { DROP 4 ; PUSH string "FA2_NOT_SUPPORTED" ; FAILWITH }
               { ITER { SWAP ;
                        DUP ;
                        DUP 3 ;
                        CAR ;
                        PAIR ;
                        DUP 4 ;
                        SWAP ;
                        EXEC ;
                        SWAP ;
                        PAIR ;
                        DUP 2 ;
                        CDR ;
                        ITER { SWAP ;
                               UNPAIR ;
                               DUP ;
                               DUP 4 ;
                               CAR ;
                               PAIR ;
                               DUP 6 ;
                               SWAP ;
                               EXEC ;
                               DUP 4 ;
                               GET 4 ;
                               DUP ;
                               DUP 5 ;
                               CAR ;
                               SUB ;
                               ISNAT ;
                               IF_NONE { PUSH string "FA2_INSUFFICIENT_BALANCE" ; FAILWITH } {} ;
                               DUP 3 ;
                               CDR ;
                               DIG 2 ;
                               DIG 3 ;
                               CAR ;
                               ADD ;
                               PAIR ;
                               PUSH nat 0 ;
                               DUP 6 ;
                               GET 3 ;
                               COMPARE ;
                               NEQ ;
                               IF { PUSH string "FA2_TOKEN_UNDEFINED" ; FAILWITH } {} ;
                               SENDER ;
                               DUP 4 ;
                               DUP 8 ;
                               CAR ;
                               GET ;
                               IF_NONE { DUP 9 } {} ;
                               CDR ;
                               DUP 2 ;
                               MEM ;
                               NOT ;
                               DUP 8 ;
                               CAR ;
                               DIG 2 ;
                               COMPARE ;
                               NEQ ;
                               OR ;
                               IF { PUSH string "FA2_NOT_OPERATOR" ; FAILWITH } {} ;
                               DIG 3 ;
                               CDR ;
                               DIG 2 ;
                               PAIR ;
                               DUG 2 ;
                               SOME ;
                               DIG 3 ;
                               CAR ;
                               UPDATE ;
                               PAIR } ;
                        UNPAIR ;
                        SWAP ;
                        SOME ;
                        DIG 2 ;
                        CAR ;
                        UPDATE } ;
                 SWAP ;
                 DIG 2 ;
                 DROP 2 ;
                 NIL operation ;
                 PAIR } }
           { DROP 4 ; PUSH string "FA2_NOT_SUPPORTED" ; FAILWITH } } }
    |}
    |> Parse.parse
    |> Compiler.compile
  in

  let storage =
    let ledgers =
      ValueMap.of_seq
      @@ List.to_seq [
        String "tz1gq9WKoVEiq69FgTrCkxDQfdJmevgHWKA7", Pair (Int (Z.of_int 4000), Set ValueSet.empty)
      ]
    in
    Map ledgers
  in
  let parameter =
    let from_ = String "tz1gq9WKoVEiq69FgTrCkxDQfdJmevgHWKA7" in
    let txs =
      List [ Pair (String "tz1gq9WKoVEiq69FgTrCkxDQfdJmevgHWKA7", Pair (Int Z.zero, Int (Z.of_int 10))) ]
    in
    let transfers = List [ Pair (from_, txs) ] in
    Union (Left (Union (Right transfers)))
  in

  let value = Runtime.run (Pair (parameter, storage)) module_ in
  Format.printf "%a\n" pp_ref value