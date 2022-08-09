type parameter = int
type storage = int

let main ((_,_):(parameter * storage)) =
  let result = 22 in 
  let result = result + 4 in
  let result = result + 4 in
  let result = result + 4 in
  // let result = a + 1 in 
  ([]: operation list), result
