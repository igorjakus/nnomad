type env
val get_variable : string -> env -> float
val create_env : (string * float) list -> env
val update_env : env -> (string * float) list -> env
val eval : env -> Expr.expr -> float
val eval_grad : env -> Expr.gradient -> (string * float) list
