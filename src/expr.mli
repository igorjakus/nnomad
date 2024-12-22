type expr =
  | Float of float
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
  | Exp of expr
  | Log of expr
  | Sin of expr
  | Cos of expr

type gradient = (string * expr) list

val ( +: ) : expr -> expr -> expr
val ( -: ) : expr -> expr -> expr
val ( *: ) : expr -> expr -> expr
val ( /: ) : expr -> expr -> expr
val ( ^: ) : expr -> expr -> expr
val ( =:= ) : expr -> expr -> bool

val string_of_expr : expr -> string
val latex_of_expr : expr -> string

val simplify_once : expr -> expr
val simplify : expr -> expr
