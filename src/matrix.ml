let a = Owl.Dense.Matrix.D.of_arrays [| [|1.; 2.|]; [|3.; 4.|] |];;
let b = Owl.Dense.Matrix.D.of_arrays [| [|5.; 6.|]; [|7.; 8.|] |];;

let c = Owl.Dense.Matrix.D.dot a b;;
Owl.Dense.Matrix.D.print c;;
