open Expr
open Derivatives
open Eval
open Optimization_types


(* The Newton-Raphson method for solving a single-variable equation. *)
let rec newton_raphson ~f ~f' ~variable ~x ~iter ~tol ~max_iter =
  let env = create_env [(variable, x)] in
  let fx  = eval env f in
  let f'x = eval env f' in
  
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
      ~x:(x -. fx /. f'x) (* x <- x - f(x)/f'(x) *)
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


(* The multivariate Newton-Raphson method for a single equation f=0 in multiple variables. *)
let newton_raphson_multivar ~f ~initial_guess ~tol ~max_iter =
  let rec loop ~env ~iter =
    let fx = eval env f in
    if Float.abs fx < tol then
      Ok (bindings env)
    else if iter >= max_iter then
      Error NoConvergence
    else
      let grad = eval_grad env (gradient f) in
      let mag2 =
        List.fold_left (fun acc (_, g) -> acc +. g *. g) 0. grad
      in
      if mag2 < 1e-14 then
        Error DivisionByZero
      else
        let step = fx /. mag2 in
        let next_env =
          List.fold_left (fun acc (var, g_val) ->
            let old_val = get_value var env in
            let new_val = old_val -. step *. g_val in
            update_env acc [var, new_val]
          ) env grad
        in
        loop ~env:next_env ~iter:(iter + 1)
  in
  loop ~env:(create_env initial_guess) ~iter:0


(* Solve a multi-variable equation using multivariable Newton method *)
let solve_newton_multivar ((lhs, rhs): equation) ~initial_guess =
  let f = lhs -: rhs in
  newton_raphson_multivar ~f ~initial_guess ~tol:1e-6 ~max_iter:100
