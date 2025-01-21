type expr =
  | Float of float
  | Var of string
  | Neg of expr
  | Sum of expr list
  | Product of expr list
  | Pow of expr * expr
  | Exp of expr
  | Log of expr
  | Sin of expr
  | Cos of expr

type gradient = (string * expr) list
type equation = expr * expr

val ( +: ) : expr -> expr -> expr
val ( -: ) : expr -> expr -> expr
val ( *: ) : expr -> expr -> expr
val ( /: ) : expr -> expr -> expr
val ( ^: ) : expr -> expr -> expr
val ( =:= ) : expr -> expr -> bool
val expr_compare : expr -> expr -> int

val get_variables : expr -> string list
val get_variable : expr -> string

val string_of_expr : expr -> string
