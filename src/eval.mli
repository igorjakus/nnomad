type env
val get_value : string -> env -> float
val create_env : (string * float) list -> env
val update_env : env -> (string * float) list -> env
val bindings : env -> (string * float) list
val eval : env -> Expr.expr -> float
val eval_grad : env -> Expr.gradient -> (string * float) list
