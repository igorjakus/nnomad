exception NoRootInInterval

val bisection :
  f:Expr.expr -> a:float -> b:float -> tolerance:float -> max_iter:int 
  -> float

val solve_bisection :
  Expr.equation -> a:float -> b:float -> tolerance:float -> max_iter:int 
  -> float