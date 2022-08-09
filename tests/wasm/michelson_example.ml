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

module Mapper = struct
  open Tezos.Michelson.Michelson_v1_primitives

  module Syntax = struct
    open Wasm.Ast

    let func var val_type instr : func' =
      Wasm.Ast.{ ftype = var; locals = val_type; body = instr }
  end

  let z_add = 144l

  let unpair = 145l

  let nil = 146l

  let pair = 147l

  let mapper (x : prim) =
    (match x with
    | I_UNPAIR -> Wasm.Operators.call Source.(unpair @@ no_region)
    | I_ADD -> Wasm.Operators.call Source.(z_add @@ no_region)
    | I_NIL -> Wasm.Operators.call Source.(nil @@ no_region)
    | I_PAIR -> Wasm.Operators.call Source.(pair @@ no_region)
    | _ -> assert false)
    |> fun x ->
    let open Source in
    x @@ no_region
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

let () =
  let open Tezos_micheline in
  let ff =
    let r, _ = Micheline_parser.tokenize contract_addition in
    let r, _ = Micheline_parser.parse_expression r in
    let r = Micheline.strip_locations r |> Micheline.root in
    r in
  let[@warning "-33"] _ =
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
        List.concat_map
          (fun x ->
            let open Micheline in
            match x with
            | Prim (_, prim, _, _) -> [Mapper.mapper prim]
            (* | Int (_, x) ->
               let open Wasm.Script in
               let open Source in
               Ast.Val (Values.Ref (ExternRef x @@ no_region) @@ no_region) *)
            | _ -> assert false)
          nodes in
      let open Source in
      let generated =
        Mapper.Syntax.func
          (Int32.of_int 0 @@ no_region)
          [Types.RefType Types.ExternRefType]
          ([Operators.local_get (0l @@ no_region) @@ no_region] @ result) in
      Wasm.Print.func stdout 0 (generated @@ no_region)
    | _ -> assert false in
  ()
