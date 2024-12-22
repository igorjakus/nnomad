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
let solve_equation ((lhs, rhs): equation) ~initial_guess =
  let f = lhs -: rhs in 
  let variable = get_variable f in
  let f' = derivative f variable in
  newton_raphson 
    ~f ~f' ~variable
    ~x:initial_guess
    ~tol:1e-6
    ~max_iter:100


(* The Newton-Raphson method for solving a system of multivariable equations. *)
(* let rec newton_raphson_multi (fs: (string * expr) list) (jacobian: gradient)
    (env: Eval.env) (tol: float) (max_iter: int) (iter: int) : Eval.env option =
  if iter >= max_iter then None
  else
    (* Evaluate the functions and Jacobian at the current environment *)
    let f_values = List.map (fun (var, f) -> var, Eval.eval env f) fs in
    let j_values = List.map (fun (var, df) -> var, Eval.eval env df) jacobian in

    (* Construct the system of equations and solve using linear algebra *)
    let deltas = Linear_algebra.solve_system f_values j_values in
    match deltas with
    | None -> None  (* No solution found *)
    | Some delta_env ->
        (* Update environment and check for convergence *)
        let new_env = Eval.update_env env delta_env in
        let max_delta = List.fold_left
          (fun acc (_, delta) -> Float.max acc (Float.abs delta)) 0.0 delta_env
        in
        if max_delta < tol then Some new_env
        else newton_raphson_multi fs jacobian new_env tol max_iter (iter + 1)


(* Solve a system of equations using the Newton-Raphson method. *)
let solve_system ~equations ~variables ~initial_guess =
  let fs = List.map (fun (lhs, rhs) -> subtract_eq lhs rhs) equations in
  let jacobian = List.map (fun var -> var, List.map (derivative var) fs) variables in
  let env = Eval.create_env initial_guess in
  let tolerance = 1e-6 in
  let max_iterations = 1000 in
  newton_raphson_multi fs jacobian env tolerance max_iterations 0 *)
