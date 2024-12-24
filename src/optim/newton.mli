val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result

type solve_error = DivisionByZero | NoConvergence | InvalidInput of string

val newton_raphson :
  f:Expr.expr ->
  f':Expr.expr ->
  variable:string ->
  x:float ->
  iter:int -> tol:float -> max_iter:int -> (float, solve_error) result

val solve_newton :
  Expr.equation -> initial_guess:float -> (float, solve_error) result
