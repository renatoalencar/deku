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

open Wasm
module VarMap = Map.Make (String)

type space = {
  mutable map : int32 VarMap.t;
  mutable count : int32;
}

let empty () = { map = VarMap.empty; count = 0l }

type types = {
  space : space;
  mutable list : Ast.type_ list;
}

let empty_types () = { space = empty (); list = [] }

type context = {
  types : types;
  tables : space;
  memories : space;
  funcs : space;
  locals : space;
  globals : space;
  datas : space;
  elems : space;
  labels : int32 VarMap.t;
  deferred_locals : (unit -> unit) list ref;
}

let empty_context () =
  {
    types = empty_types ();
    tables = empty ();
    memories = empty ();
    funcs = empty ();
    locals = empty ();
    globals = empty ();
    datas = empty ();
    elems = empty ();
    labels = VarMap.empty;
    deferred_locals = ref [];
  }

module EmptyMod = struct
  (* creates an emptyu module  *)
  (* let empty_module =
     {
       types = [];
       globals = [];
       tables = [];
       memories = [];
       funcs = [];
       start = None;
       elems = [];
       datas = [];
       imports = [];
       exports = [];
     }
  *)
  let create_empty () = Wasm.Ast.empty_module
end

let body fmtr =
  (* let main_func =
       {|
     (func (export "main") (param $tup i32) (result i32)
       call $nil
       local.get $tup
       call $unpair
       call $z_add
       local.tee $tup
       call $pair
     )
     |}
     in *)
  Fmt.(
    str
      {|
(module 
  (import "env" "unpair" (func $unpair (param i32) (result i32 i32)))
  (import "env" "pair" (func $pair (param i32 i32) (result i32)))
  (import "env" "z_add" (func $z_add (param i32 i32) (result i32)))
  (import "env" "nil" (func $nil (result i32)))
  (memory (export "memory") 1)
  %s
)
|}
      fmtr)

module Mapper = struct
  open Tezos.Michelson.Michelson_v1_primitives

  let mapper (x : prim) =
    (match x with
    | I_UNPAIR -> "call $unpair"
    | I_ADD -> "call $z_add"
    | I_NIL -> "call $nil"
    | I_PAIR -> "call $pair"
    | _ -> assert false)
    |> fun x -> x
end

let contract_addition =
  (*
     type storage = int

        type return = operation list * storage


        (* Main access point that dispatches to the entrypoints according to
           the smart contract parameter. *)

        let main (action, store : int * storage) : return =   // No operations
         ([]),(action + store)
  *)
  {|
  
  { parameter int ;
  storage int ;
  code { UNPAIR ; ADD ; NIL operation ; PAIR } }

  |}

module Externs = struct
  open Wasm

  module Value = struct
    type t = Values.value
  end

  type value_type =
    | I32
    | I64

  type func_type = value_type list * value_type list option

  type t =
    | Func of func_type * (Memory.t -> Value.t list -> Value.t list option)

  let func typ func = Func (typ, func)

  let to_wasm memory t =
    let wrap func memory args =
      let memory = memory () in
      func memory args |> Option.to_list |> List.flatten in
    let open Wasm in
    let type_to_wasm_type t =
      Types.NumType
        (match t with
        | I32 -> I32Type
        | I64 -> I64Type) in
    match t with
    | Func ((params, ret), func) ->
      let func_type =
        let params = List.map type_to_wasm_type params in
        let ret =
          ret
          |> Option.map (List.map type_to_wasm_type)
          |> Option.to_list
          |> List.flatten in
        Types.FuncType (params, ret) in
      Instance.ExternFunc (Func.alloc_host func_type (wrap func memory))
end

let memory = Utf8.decode "memory"

let get_memory t =
  match Instance.export t memory with
  | Some (ExternMemory memory) -> memory
  | _ -> failwith "no memory"

module M = Map.Make (Int32)
open Helpers

module Z = struct
  include Z

  let pp fmt t = Format.fprintf fmt "%s" (Z.to_string t)
end

type values =
  | Pair : int32 * int32 -> values
  | List : int32 list -> values
  | Num  : Z.t -> values

let rec pp_values table fmt = function
  | Pair (x, y) ->
    let pp = pp_values table in
    Format.fprintf fmt "(Pair %a, %a)" pp (M.find x table) pp (M.find y table)
  | List x ->
    let pp = pp_values table in
    let lst = List.map (fun x -> M.find x table) x in
    Format.fprintf fmt "List [%a]" (Fmt.list pp) lst
  | Num z -> Format.fprintf fmt "Num %s" (Z.to_string z)

let incrr r = r := Int32.succ !r

let make ~gas ~module_ ~custom =
  let empty = ref M.empty in
  let counter = ref 0l in
  empty := M.add !counter (Num (Z.of_int 5)) !empty;
  incrr counter;
  empty := M.add !counter (Num (Z.of_int 5)) !empty;
  incrr counter;
  let empty = ref @@ M.add !counter (Pair (0l, 1l)) !empty in
  incrr counter;
  let open Core in
  let _custom =
    let custom = custom in
    Externs.[func ([I64], Some [I32]) custom] in
  let[@warning "-8"] z_add =
    let adder _ [Values.Num (I32 one); Values.Num (I32 two)] =
      let[@warning "-8"] (Num ptr1) = M.find one !empty in
      let[@warning "-8"] (Num ptr2) = M.find two !empty in
      empty := M.add !counter (Num (Z.add ptr1 ptr2)) !empty;
      let ret = !counter in
      incrr counter;
      Some [Values.Num (I32 ret)] in
    Externs.[func ([I32; I32], Some [I32]) adder] in
  let[@warning "-8"] pair =
    let adder _ [Values.Num (I32 one); Values.Num (I32 two)] =
      empty := M.add !counter (Pair (one, two)) !empty;
      let ret = !counter in
      incrr counter;
      Some [Values.Num (I32 ret)] in
    Externs.[func ([I32; I32], Some [I32]) adder] in
  let[@warning "-8"] nil =
    let adder _ [] =
      empty := M.add !counter (List []) !empty;
      let ret = !counter in
      incrr counter;
      Some [Values.Num (I32 ret)] in
    Externs.[func ([], Some [I32]) adder] in
  let[@warning "-8"] unpair =
    let adder _ [Values.Num (I32 one)] =
      let[@warning "-8"] (Pair (one, two)) = M.find one !empty in
      Some [Values.Num (I32 one); Values.Num (I32 two)] in
    Externs.[func ([I32], Some [I32; I32]) adder] in
  let uninit : Wasm.Instance.module_inst Set_once.t = Set_once.create () in
  let imports =
    List.map
      ~f:
        (Externs.to_wasm (fun () ->
             let instance = Set_once.get_exn uninit Lexing.dummy_pos in
             get_memory instance))
      (unpair @ pair @ z_add @ nil) in
  let instance = Eval.init gas module_ imports in
  (* XXX: Is this the better way of doing this? *)
  Set_once.set_exn uninit Lexing.dummy_pos instance;
  (instance, empty)

let exports =
  let open Wasm in
  let open Source in
  [
    Ast.
      {
        name = Utf8.decode "memory";
        edesc = Ast.MemoryExport (1l @@ no_region) @@ no_region;
      }
    @@ no_region;
  ]

let run fmt =
  Wasm.Parse.string_to_module (body fmt) |> function
  | { it = Textual m; at = _ } ->
    Wasm.Print.module_ stdout 80 m;
    let instance, table =
      make ~gas:(ref max_int) ~module_:m ~custom:(fun _ _ -> assert false) in
    let func_inst =
      Wasm.Instance.export instance (Utf8.decode "main") |> Option.get
      |> function
      | ExternFunc x -> x
      | _ -> assert false in
    let[@warning "-8"] [Values.Num (I32 x)] =
      Wasm.Eval.invoke (ref max_int) func_inst [Values.Num (I32 2l)] in
    let _result = M.find x !table in
    let pp = pp_values !table in
    Format.printf "result is %a\n" pp _result;
    ()
  | _ -> assert false

let () =
  let open Tezos_micheline in
  let ff =
    let r, _ = Micheline_parser.tokenize contract_addition in
    let r, _ = Micheline_parser.parse_expression r in
    let r = Micheline.strip_locations r |> Micheline.root in
    r in
  let[@warning "-33"] ff =
    let open Tezos.Michelson.Michelson_v1_primitives in
    match ff with
    | Seq (_, [Prim _; Prim _; Prim (_, "code", [Seq (_, nodes)], _)]) ->
      let nodes =
        List.map
          (fun x ->
            Tezos_micheline.Micheline.map_node Fun.id (fun x ->
                Tezos.Michelson.Michelson_v1_primitives.prim_of_string x
                |> Result.get_ok)
            @@ Micheline.root
            @@ Micheline.strip_locations x)
          nodes in
      let result =
        List.map
          (fun x ->
            let open Micheline in
            match x with
            | Prim (_, prim, _, _) -> Mapper.mapper prim
            (* | Int (_, x) ->
               let open Wasm.Script in
               let open Source in
               Ast.Val (Values.Ref (ExternRef x @@ no_region) @@ no_region) *)
            | _ -> assert false)
          nodes in
      Format.asprintf
        {| 
        (func (export "main") (param $tup i32) (result i32)
           %a
         )
      |}
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
           Fmt.string)
        (["local.get 0"] @ result)
    | _ -> assert false in
  run ff
