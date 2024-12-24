val gradient_descent :
  expr:Expr.expr ->
  env:Eval.env -> learning_rate:float -> iterations:int -> Eval.env

val solve_gd :
  Expr.equation ->
  initial_guess:Eval.env -> lr:float -> iterations:int -> Eval.env
