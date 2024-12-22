(* solver.ml *)
open Expr
open Derivatives
let (>>=) = Result.bind

type equation = expr * expr
type solve_error = DivisionByZero | NoConvergence | InvalidInput of string


(* The Newton-Raphson method for solving a single-variable equation. *)
let rec newton_raphson ~f ~f' ~variable ~x ~iter ~tol ~max_iter =
  let env = Eval.create_env [(variable, x)] in
  let fx  = Eval.eval env f in
  let f'x = Eval.eval env f' in
  
  (if iter >= max_iter 
   then Error NoConvergence 
   else Ok ()) >>= fun () ->
  
  (if Float.abs f'x < tol 
   then Error DivisionByZero 
   else Ok ()) >>= fun () ->
    
  if Float.abs fx < tol 
  then Ok x
  else newton_raphson 
         ~f ~f' ~variable 
         ~x:(x -. fx /. f'x) 
         ~iter:(iter + 1)
         ~tol ~max_iter 


(* Solve a single-variable equation using the Newton-Raphson method. *)
let solve_newton ((lhs, rhs): equation) ~initial_guess =
  let f = lhs -: rhs in 
  let variable = get_variable f in
  let f' = derivative f variable in
  newton_raphson 
    ~f ~f' ~variable
    ~x:initial_guess
    ~iter:0
    ~tol:1e-6
    ~max_iter:100
