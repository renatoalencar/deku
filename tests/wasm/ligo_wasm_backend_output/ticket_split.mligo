//  (module
//                                  (import "env" "syscall" (func $syscall (param i64) (result i32)))
//                                  (memory (export "memory") 1)
//                                  (func (export "main")  (param i32) (result i64 i64 i64)
//                                    i32.const 5
//                                    i32.const 4
//                                    i32.store
//                                    i32.const 10
//                                    i32.const 0
//                                    i32.store
//                                    i32.const 15
//                                    i64.const 5
//                                    i64.store
//                                    i32.const 24
//                                    i64.const 5
//                                    i64.store
//                                    i64.const 5
//                                    call $syscall
//                                    i32.const 95
//                                    i32.add
//                                    i32.const 5
//                                    i32.store
//                                    i32.const 105
//                                    i32.const 5
//                                    i32.store
//                                    i64.const 100
//                                    call $syscall
//                                    i32.const 6
//                                    i32.add
//                                    i32.const 5
//                                    i32.store
//                                    i32.const 111
//                                    i32.const 9
//                                    i32.store
//                                    i64.const 106
//                                    call $syscall
//                                    i32.const 5
//                                    i32.sub
//                                    i64.extend_i32_s
//                                    (i64.const 10)
//                                    (i64.const 555)

type parameter = int
type storage = int

let main ((_,_):(parameter * storage)) =
    let a = Tezos.create_ticket 1 10n in
    match Tezos.split_ticket a (5n, 5n) with 
      Some split_tickets -> ([]: operation list), 1
    | None -> ([]: operation list), 0


    