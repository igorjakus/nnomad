open Expr
open Simplify
open Eval
open Derivatives
open Optimization_types


(* Perform gradient descent to minimize the expression *)
let gradient_descent ~expr ~env ~learning_rate ~max_iter =
  (if max_iter <= 0 then
    Error (InvalidInput "max_iter must be positive")
   else Ok ()) >>= fun () ->
  
  (if learning_rate <= 0.0 then
    Error (InvalidInput "Learning rate must be positive")
   else Ok ()) >>= fun () ->

  (* Compute new values for all variables based on gradients *)
  let calculate_updated_values ~expr ~env ~learning_rate =
    let updated_value (var, f') = 
      let f'_value = eval env f' in
      let current_value = get_value var env in
      (var, current_value -. learning_rate *. f'_value)  (* x - lr * f'(env) *)
    in
    List.map updated_value (gradient expr) in
    
  let gradient_descent_step ~expr ~env ~learning_rate =
    let updates = calculate_updated_values ~expr ~env ~learning_rate in
    update_env env updates in

  let rec loop env i =
    if i >= max_iter then 
      Ok env
    else 
      loop (gradient_descent_step ~expr ~env ~learning_rate) (i + 1)
  in
  
  loop env 0


(* Solve an equation using gradient descent *)
let solve_gradient_descent ((lhs, rhs): equation) ~initial_guess ~learning_rate ~max_iter = 
  let f = (lhs -: rhs) in
  let f_sq = simplify (f *: f) in 
  gradient_descent 
    ~expr:f_sq
    ~env:initial_guess
    ~learning_rate
    ~max_iter
