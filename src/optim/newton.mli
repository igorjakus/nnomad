open Optimization_types

val newton_raphson :
  f:Expr.expr ->
  f':Expr.expr ->
  variable:string ->
  x:float ->
  iter:int -> 
  tol:float ->
  max_iter:int ->
  (float, optim_error) result

val solve_newton :
  Expr.equation ->
  initial_guess:float ->
  max_iter:int ->
  (float, optim_error) result

val newton_multivariable :
  f:Expr.expr ->
  grad:Expr.gradient ->
  env:Eval.env ->
  iter:int ->
  tol:float ->
  max_iter:int ->
  (Eval.env, optim_error) result

val solve_newton_multivar :
  Expr.equation ->
  initial_guess:Eval.env ->
  max_iter:int ->
  (Eval.env, optim_error) result
