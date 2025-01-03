open Expr

(* Environment type for variable bindings *)
type env = (string * float) list


(* Get the value of a variable from the environment *)
let get_value var (env: env) =
  List.assoc var env


(* Update the environment with new variable values *)
let update_env (env: env) (updates: env) =
  updates @ List.filter (fun (k, _) -> not (List.mem_assoc k updates)) env


(* Evaluates an expression given an environment mapping variables to values *)
let rec eval (env: env) (expr: expr): float =
  match simplify expr with
  | Float x -> x
  | Var s -> get_value s env
  | Add (a, b)  -> eval env a +. eval env b
  | Sub (a, b)  -> eval env a -. eval env b
  | Mult (a, b) -> eval env a *. eval env b
  | Div (a, b)  -> eval env a /. eval env b
  | Pow (a, n)  -> Float.pow (eval env a) (eval env n)
  | Exp a -> Float.exp (eval env a)
  | Log a -> Float.log (eval env a)
  | Sin a -> Float.sin (eval env a)
  | Cos a -> Float.cos (eval env a)


(* Evaluates an expression at a specific value assuming it is single-var expr *)
let rec eval_at (value: float) (expr: expr): float =
  match simplify expr with
  | Float x -> x
  | Var _ -> value  (* ignore what variable it is, substitude with val *)
  | Add (a, b)  -> eval_at value a +. eval_at value b
  | Sub (a, b)  -> eval_at value a -. eval_at value b
  | Mult (a, b) -> eval_at value a *. eval_at value b
  | Div (a, b)  -> eval_at value a /. eval_at value b
  | Pow (a, n)  -> Float.pow (eval_at value a) (eval_at value n)
  | Exp a -> Float.exp (eval_at value a)
  | Log a -> Float.log (eval_at value a)
  | Sin a -> Float.sin (eval_at value a)
  | Cos a -> Float.cos (eval_at value a)


(* Evaluates a gradient given an environment mapping variables to values *)
let eval_grad (env: env) (gradient: gradient) =
  List.map (fun (var, expr) -> (var, eval env expr)) gradient
