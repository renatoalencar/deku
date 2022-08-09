type parameter = int
type storage = int

let main ((_,_):(parameter * storage)) =
  let result = 22 in 
  let result = result + 4 in
  let result = result + 4 in
  let result = result + 4 in
  // let result = a + 1 in 
  ([]: operation list), result

(** 
define region and derive cstruct ppx.
 type t = {
  offset: uint32_t;
  lenght: uint32_t;
  capacity: uint32_t;
 };;
*)
(**
  What to do with transaction param??? 
  use mutable global for 
  1. static data segments
  2. heap memory offset. 
  3. simulate stack as needed. 
  only container, raw ptr accepted.
   - records 
   - maps 
   - lists
   - pairs
   - tickets
  all are externrefs enabled with reference types proposal
*)

(**
  compile basic michelson to webassembly text format. 
  compile michelson data structures  to deku style syscalls.
   - tup => externref 
   - record => tup 
   - array => externref 
   - list => externref 
   - bytes => region
   - string => bytes 
   - map => externref 
   - bigmap => externref the same as map rn.
   - for rust specific data_structures => all are externrefs. (vec <=> list, maps/hashmaps <=> map)
    ```rust 
    #[repr(transparent)] //this is important to ensure it being just an external reference
    pub struct External(u32)
    ```
  - drop rust-micheline for now  and forget about it, forget about micheline at all. pack and unpack for free now from ocaml.
  - use rawbuffers/regions for variable sized data-structures.
  define `malloc`/`consume` 
   - `malloc` => (allocate a region struct, bump the offset in memory, return the offset to free region).
   - `consume` => (consume bytes at the region).
*)
