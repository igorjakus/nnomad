open Eval
open Expr
open Optimization_types



let bisection ~f ~a ~b ~tolerance ~max_iter =
  if tolerance <= 0.0 then
    Error (InvalidInput "Tolerance must be positive")
  else if max_iter <= 0 then
    Error (InvalidInput "Max iterations must be positive")
  else

  let variable = get_variable f in
  let is_zero x = abs_float x < tolerance in
  let env = create_env [] in  (* make env only once *)
  let eval_at x = eval (update_env env [(variable, x)]) f in

  let rec loop a b iter =
    if iter > max_iter then
      Error MaxIterationsReached
    else
      let fa = eval_at a in
      let fb = eval_at b in
      let mid = (a +. b) /. 2.0 in
      let fmid = eval_at mid in

      if is_zero fmid then
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
