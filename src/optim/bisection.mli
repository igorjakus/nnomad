open Optimization_types

val bisection :
  f:Expr.expr ->
  a:float ->
  b:float ->
  tolerance:float ->
  max_iter:int ->
  (float, optim_error) result

val solve_bisection :
  Expr.equation ->
  a:float ->
  b:float ->
  tolerance:float ->
  max_iter:int ->
  (float, optim_error) result