(* Automatic Differentiation module implementing forward-mode AD *)

(* Represents a typed expression tree for automatic differentiation. *)
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

  
(* Convert expression to string for debugging and visualization *)
let rec to_string = function
  | Float x -> string_of_float x
  | Var s -> s
  | Add (a, b)  -> "(" ^ to_string a ^ " + " ^ to_string b ^ ")"
  | Sub (a, b)  -> "(" ^ to_string a ^ " - " ^ to_string b ^ ")"
  | Mult (a, b) -> "(" ^ to_string a ^ " * " ^ to_string b ^ ")"
  | Div (a, b)  -> "(" ^ to_string a ^ " / " ^ to_string b ^ ")"
  | Pow (a, n)  -> "(" ^ to_string a ^ "^"   ^ to_string n ^ ")"
  | Exp a -> "exp(" ^ to_string a ^ ")"
  | Log a -> "log(" ^ to_string a ^ ")"
  | Sin a -> "sin(" ^ to_string a ^ ")"
  | Cos a -> "cos(" ^ to_string a ^ ")"


(* Operator overloading for more natural expression syntax *)
let ( +: ) a b = Add (a, b)
let ( -: ) a b = Sub (a, b)
let ( *: ) a b = Mult (a, b)
let ( /: ) a b = Div (a, b)
let ( ^: ) x n = Pow (x, n)


(* Simplifies expressions by applying basic algebraic rules *)
let rec simplify = function
  (* Error handling *)
  | Div (_, Float 0.) -> failwith "division by zero"
  | Log (Float x) when x <= 0. -> failwith "log of non-positive number"
  
  (* Calculating values*)
  | Mult (Float 0., _) -> Float 0.
  | Mult (_, Float 0.) -> Float 0.
  | Div  (Float 0., _) -> Float 0.
  | Pow  (_, Float 0.) -> Float 1. 
  | Add  (Float a, Float b) -> Float (a +. b)
  | Sub  (Float a, Float b) -> Float (a -. b)
  | Mult (Float a, Float b) -> Float (a *. b)
  | Div  (Float a, Float b) -> Float (a /. b)
  | Pow  (Float a, Float b) -> Float (a ** b)
  | Log (Float x) -> Float (log x)
  | Exp (Float x) -> Float (exp x)
  | Sin (Float x) -> Float (sin x)
  | Cos (Float x) -> Float (cos x)

  (* Trivial simplification *)
  | Add  (Float 0., x) -> simplify x 
  | Add  (x, Float 0.) -> simplify x
  | Sub  (x, Float 0.) -> simplify x
  | Mult (Float 1., x) -> simplify x
  | Mult (x, Float 1.) -> simplify x
  | Div  (x, Float 1.) -> simplify x
  | Pow  (x, Float 1.) -> simplify x

  (* Unique function properties *)
  | Log (Exp x) -> simplify x
  | Exp (Log x) -> simplify x

  (* Recursively simplify subexpressions *)
  | Add  (a, b) -> Add (simplify a, simplify b)
  | Sub  (a, b) -> Sub (simplify a, simplify b)
  | Mult (a, b) -> Mult (simplify a, simplify b)
  | Div  (a, b) -> Div (simplify a, simplify b)
  | Pow  (a, b) -> Pow (simplify a, simplify b)
  | Exp  a -> Exp (simplify a)
  | Log  a -> Log (simplify a)
  | Sin  a -> Sin (simplify a)
  | Cos  a -> Cos (simplify a)

  (* Base cases *)
  | Float x -> Float x
  | Var x -> Var x


(* Environment type for variable bindings *)
module StringMap = Map.Make(String)
type env = float StringMap.t


(* Create env from list of string * float pairs *)
let create_env (bindings: (string * float) list) =
  List.fold_left (fun acc (var, value) -> StringMap.add var value acc) StringMap.empty bindings


(* Update the environment with new variable values *)
let update_env env updates =
  List.fold_left
    (fun acc (var, value) -> StringMap.add var value acc)
    env updates


(* Evaluates an expression given an environment mapping variables to values *)
let rec eval (env: env) (expr: expr): float =
  match simplify expr with
  | Float x -> x
  | Var s -> StringMap.find s env
  | Add (a, b)  -> eval env a +. eval env b
  | Sub (a, b)  -> eval env a -. eval env b
  | Mult (a, b) -> eval env a *. eval env b
  | Div (a, b)  -> eval env a /. eval env b
  | Pow (a, n)  -> Float.pow (eval env a) (eval env n)
  | Exp a -> Float.exp (eval env a)
  | Log a -> Float.log (eval env a)
  | Sin a -> Float.sin (eval env a)
  | Cos a -> Float.cos (eval env a)


(* Computes the derivative of an expression with respect to a variable *)
let rec derivative expr var =
  match expr with
  | Float _ -> Float 0.
  | Var x when x = var -> Float 1.
  | Var _ -> Float 0.
  | Add (f, g) -> derivative f var +: derivative g var
  | Sub (f, g) -> derivative f var -: derivative g var
  | Mult (f, g) -> (derivative f var *: g) +: (f *: derivative g var)
  | Div (f, g) ->
      let num = (derivative f var *: g) -: (f *: derivative g var) in
      let den = g *: g in
      num /: den 
  | Pow (f, n) ->
      let n' = n -: Float 1. in 
      let power = f ^: n' in
      n *: power *: derivative f var
  | Exp f -> Exp f *: derivative f var                 (* Chain rule with exp *)
  | Log f -> derivative f var /: f                     (* Chain rule with log *)
  | Sin f -> Cos f *: derivative f var                 (* Chain rule with sin *)
  | Cos f -> Float (-1.) *: Sin f *: derivative f var  (* Chain rule with cos *)
  

(* Compute gradient as partial derivatives with respect to all variables *)
let gradient expr =
  (* Get all unique variables in an expression *)
  let rec get_vars expr = match expr with
    | Var x -> [x]
    | Float _ -> []
    | Add (a, b) | Sub (a, b) | Mult (a, b) 
    | Div (a, b) | Pow (a, b) -> List.sort_uniq String.compare (get_vars a @ get_vars b)
    | Exp a | Log a | Sin a | Cos a -> get_vars a
  in

  let vars = get_vars expr in
  List.map (fun var -> (var, derivative expr var)) vars


(* Computes nth derivative of an expression with respect to a variable *)
let rec nth_derivative expr var n =
  if n < 0 then invalid_arg "nth_derivative: negative order"
  else if n = 0 then expr
  else nth_derivative (derivative expr var) var (n - 1)
