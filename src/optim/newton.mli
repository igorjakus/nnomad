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
  (float, optim_error) result
