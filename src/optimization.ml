open Eval
open Derivatives


(* Perform gradient descent to minimize the expression *)
let gradient_descent ~expr ~env ~learning_rate ~iterations =
  
  (* Compute new values for all variables based on gradients *)
  let calculate_updated_values ~expr ~env ~learning_rate =
    let updated_value (var, f') = 
      let f'_value = eval env f' in
      let current_value = StringMap.find var env in
      (var, current_value -. learning_rate *. f'_value)  (* x - lr * f'(env) *)
    in
    List.map updated_value (gradient expr) in

  (* Perform one iteration of gradient descent *)
  let gradient_descent_step ~expr ~env ~learning_rate =
    let updates = calculate_updated_values ~expr ~env ~learning_rate in
    update_env env updates in
  
  (* Make n=iterations steps in gradient descent *)
  let rec loop env i =
    if i >= iterations then env
    else loop (gradient_descent_step ~expr ~env ~learning_rate) (i + 1)
  in
  
  loop env 0

