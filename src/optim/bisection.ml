open Eval
open Expr

exception NoRootInInterval


(* Function implementing the bisection method *)
let bisection ~f ~a ~b ~tolerance ~max_iter =
  let variable = get_variable f in
  let is_zero x = abs_float x < tolerance in
  let eval_at x = eval (create_env [(variable, x)]) f in

  let rec loop a b iter =
    if iter > max_iter then
      failwith "Max iterations reached"
    else
      let fa = eval_at a in
      let fb = eval_at b in
      let mid = (a +. b) /. 2.0 in
      let fmid = eval_at mid in

      if is_zero fmid then
        mid
      else if fa *. fmid < 0.0 then
        loop a mid (iter + 1)
      else if fmid *. fb < 0.0 then
        loop mid b (iter + 1)
      else
        raise NoRootInInterval
  in
  loop a b 0
