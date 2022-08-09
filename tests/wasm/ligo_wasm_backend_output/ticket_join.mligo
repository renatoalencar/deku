// (module
//                     (import "env" "syscall" (func $syscall (param i64) (result i32)))
//                     (memory (export "memory") 1)
//                     (func (export "main")  (param i32) (result i64 i64 i64)

//                       i32.const 10 
//                       i32.const 6
//                       i32.store      ;; store ticket_join
//                       i32.const 15
//                       i32.const 0
//                       i32.store     ;; store self

//                       i32.const 20
//                       i32.const 4
//                       i32.store
//                       i64.const 10 ;; memory address to ticket_join
//                       call $syscall
//                       i32.const 5
//                       i32.add
//                       i32.const 5 
//                       i32.store
//                       i32.const 20
//                       i32.const 10
//                       i32.store
//                       i64.const 15 ;; memory address to self
//                       call $syscall
//                       i64.extend_i32_s
//                       (i64.const 8)
//                       (i64.const 555)
//                       ))

type parameter = int
type storage = int

let main ((_,_):(parameter * storage)) =
    let a = Tezos.create_ticket 1 10n in
    let b = Tezos.create_ticket 1 10n in
    let _joined = Tezos.join_tickets (a, b) in
    ([]: operation list), 0


    