type env = (string * float) list
val get_value : string -> env -> float
val update_env : env -> env -> env
val eval : env -> Expr.expr -> float
val eval_at : float -> Expr.expr -> float
val eval_grad : env -> Expr.gradient -> env
val print_env : env -> unit
