open Expr
open Derivatives
open Eval
open Optimization_types


(* The Newton-Raphson method for solving a single-variable equation. *)
let rec newton_raphson ~f ~f' ~variable ~x ~iter ~tol ~max_iter =
  (if max_iter <= 0 then
    Error (InvalidInput "max_iter must be positive")
   else Ok ()) >>= fun () ->
  
  (if tol <= 0.0 then
    Error (InvalidInput "Tolerance must be positive")
   else Ok ()) >>= fun () ->
  
  let fx  = eval_at x f in
  let f'x = eval_at x f' in
  
  (if iter >= max_iter then 
    Error NoConvergence
   else Ok ()) >>= fun () ->
  
  (if Float.abs f'x < tol then 
    Error DivisionByZero
   else Ok ()) >>= fun () ->
  
  if Float.abs fx < tol then
    Ok x
  else 
    newton_raphson 
      ~f ~f' ~variable 
      ~x:(x -. fx /. f'x)  (* x <- x - f(x)/f'(x) *)
      ~iter:(iter + 1)
      ~tol ~max_iter


(* Solve a single-variable equation using the Newton-Raphson method. *)
let solve_newton ((lhs, rhs): equation) ~initial_guess ~max_iter =
  let f = lhs -: rhs in 
  let variable = get_variable f in
  let f' = derivative f variable in
  newton_raphson 
    ~f ~f' ~variable
    ~x:initial_guess
    ~iter:0
    ~tol:1e-6
    ~max_iter


(* The multivariate Newton-Raphson method for a single equation f=0 in multiple variables. *)
let rec newton_multivariable ~f ~grad ~env ~iter ~tol ~max_iter =
  (if max_iter <= 0 then
    Error (InvalidInput "max_iter must be positive")
   else Ok ()) >>= fun () ->
  
  (if tol <= 0.0 then
    Error (InvalidInput "Tolerance must be positive")
   else Ok ()) >>= fun () ->
  
  let fx = eval env f in

  (* Check for convergence *)
  if Float.abs fx < tol then
    Ok env
  else if iter >= max_iter then
    Error NoConvergence
  else
    (* Evaluate the gradient at the current environment *)
    let grad_vals = eval_grad env grad in

    (* Calculate the magnitude squared of the gradient *)
    let grad_magnitude_squared =
      List.fold_left (fun acc (_, g) -> acc +. g *. g) 0. grad_vals
    in

    (* Check for division by zero *)
    (if grad_magnitude_squared < 1e-14 then 
      Error DivisionByZero
     else Ok ()) >>= fun () ->

    (* Calculate the step size *)
    let step = fx /. grad_magnitude_squared in

    (* Update the environment with the new values *)
    let next_env =
      List.map (fun (var, g_val) ->
        let old_val = get_value var env in
        let new_val = old_val -. step *. g_val in  (* x <- x - f(x)/f'(x) * grad_magnitude *)
        (var, new_val)
      ) grad_vals
    in
    newton_multivariable 
      ~f ~grad ~env:(update_env env next_env) 
      ~iter:(iter + 1) 
      ~tol ~max_iter


(* Solve a multi-variable equation using multivariable Newton method *)
let solve_newton_multivar ((lhs, rhs): equation) ~initial_guess ~max_iter =
  let f = lhs -: rhs in
  let grad = gradient f in
  newton_multivariable 
    ~f ~grad
    ~env:initial_guess
    ~iter:0
    ~tol:1e-6
    ~max_iter

