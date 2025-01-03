open Eval
open Expr
open Optimization_types


let bisection ~f ~a ~b ~tolerance ~max_iter =
  (if tolerance <= 0.0 then
    Error (InvalidInput "Tolerance must be positive")
   else Ok ()) >>= fun () ->
  
  (if max_iter <= 0 then
    Error (InvalidInput "Max iterations must be positive")
   else Ok ()) >>= fun () ->

  let rec loop a b iter =
    if iter > max_iter then
      Error MaxIterationsReached
    else
      let fa = eval_at a f in
      let fb = eval_at b f in
      let mid = (a +. b) /. 2.0 in
      let fmid = eval_at mid f in

      if abs_float fmid < tolerance then
        Ok mid
      else if fa *. fmid < 0.0 then
        loop a mid (iter + 1)
      else if fmid *. fb < 0.0 then
        loop mid b (iter + 1)
      else
        Error NoRootInInterval
  in
  loop a b 0


(* Function to solve a single-variable equation using the bisection method *)
let solve_bisection ((lhs, rhs): equation) ~a ~b ~tolerance ~max_iter =
  let f = lhs -: rhs in
  bisection ~f ~a ~b ~tolerance ~max_iter
