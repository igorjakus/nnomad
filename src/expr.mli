type expr =
  | Float of float
  | Var   of string           (* Named variable *)
  | Add   of expr * expr      (* Addition *)
  | Sub   of expr * expr      (* Subtraction *)
  | Mult  of expr * expr      (* Multiplication *)
  | Div   of expr * expr      (* Division *)
  | Pow   of expr * expr      (* Power with constant exponent *)
  | Exp   of expr             (* Exponential function *)
  | Log   of expr             (* Natural logarithm *)
  | Sin   of expr             (* Sine function *)
  | Cos   of expr             (* Cosine function *)
  | Lazy  of (unit -> expr)   (* Lazy evaluation *)

type gradient = (string * expr) list

type equation = expr * expr

val ( +: ) : expr -> expr -> expr
val ( -: ) : expr -> expr -> expr
val ( *: ) : expr -> expr -> expr
val ( /: ) : expr -> expr -> expr
val ( ^: ) : expr -> expr -> expr
val ( =:= ) : expr -> expr -> bool

val get_variables : expr -> string list
val get_variable : expr -> string

val string_of_expr : expr -> string
val latex_of_expr : expr -> string

val simplify_once : expr -> expr
val simplify : expr -> expr

val lazy_expr : (unit -> expr) -> expr
