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


(* Operator overloading for more natural expression syntax *)
let ( +: ) a b = Add (a, b)
let ( -: ) a b = Sub (a, b)
let ( *: ) a b = Mult (a, b)
let ( /: ) a b = Div (a, b)
let ( ^: ) x n = Pow (x, n)

let rec (=:=) e1 e2 =
  match e1, e2 with
  | Float a, Float b -> abs_float (a -. b) < 1e-10
  | Var a,   Var b   -> a = b
  
  | Add (a1, b1),  Add (a2, b2) 
  | Sub (a1, b1),  Sub (a2, b2)
  | Mult (a1, b1), Mult (a2, b2)
  | Div (a1, b1),  Div (a2, b2)
  | Pow (a1, b1),  Pow (a2, b2) -> a1 =:= a2 && b1 =:= b2

  | Exp a1, Exp a2 
  | Log a1, Log a2
  | Sin a1, Sin a2 
  | Cos a1, Cos a2 -> a1 =:= a2
  
  | _ -> false


(* Helper function to apply simplify recursively only once *)
let rec simplify_once expr = 
  match expr with
  (* Error handling *)
  | Div (_, Float 0.)          -> failwith "division by zero"
  | Log (Float x) when x <= 0. -> failwith "log of non-positive number"

  (* Base cases *)
  | Float x -> Float x
  | Var x -> Var x

  (* Calculating constant values *)
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

  (* Now we are sure that matched arguments of expressions aren't only constants.
  It's important for many simplifications so we won't get into infinite recursion! *)

  (* Identity operations *)
  | Add  (Float 0., x) -> simplify_once x 
  | Add  (x, Float 0.) -> simplify_once x
  | Sub  (x, Float 0.) -> simplify_once x
  | Mult (Float 1., x) -> simplify_once x
  | Mult (x, Float 1.) -> simplify_once x
  | Div  (x, Float 1.) -> simplify_once x
  | Pow  (x, Float 1.) -> simplify_once x
  | Log (Exp x)        -> simplify_once x
  | Exp (Log x)        -> simplify_once x

  (* Sort expressions *)
  | Mult (y, Float x) -> simplify_once (Mult (Float x, y))
  | Add  (Float x, y) -> simplify_once (Add(y, Float x))
  (* Since we matched all-float expressions already, we are sure y is not Float.
  Therefore this part of code is infinite-recursion safe *)

  (* Other algebraic simplifications *)
  | Sub (x, y)          when x =:= y -> Float 0.
  | Sub (x, Float y)    when y < 0.  -> simplify_once (x +: Float (-.y))
  | Add (x, y)          when x =:= y -> simplify_once (Float 2. *: x)

  | Mult(Pow (x, n), y) when x =:= y -> simplify_once (x ^: (n +: Float 1.))
  | Mult(y, Pow(x, n))  when x =:= y -> simplify_once (x ^: (n +: Float 1.))
  | Div(Pow (x, n), y)  when x =:= y -> simplify_once (x ^: (n -: Float 1.))
  | Div(y, Pow (x, n))  when x =:= y -> simplify_once (x ^: (Float 1. -: n))
  
  | Add(Log(x), Log(y)) -> simplify_once (Log(x *: y))
  | Sub(Log(x), Log(y)) -> simplify_once (Log(x /: y))

  (* Recursively simplify complex expressions *)
  | Add (a, b) ->
    let a' = simplify_once a in
    let b' = simplify_once b in
    (Add (a', b'))
  | Sub (a, b) ->
    let a' = simplify_once a in
    let b' = simplify_once b in
    (Sub (a', b'))
  | Mult (a, b) ->
    let a' = simplify_once a in
    let b' = simplify_once b in
    (Mult (a', b'))
  | Div (a, b) ->
    let a' = simplify_once a in
    let b' = simplify_once b in
    (Div (a', b'))
  | Pow (a, b) ->
    let a' = simplify_once a in
    let b' = simplify_once b in
    (Pow (a', b'))

  (* Recursively simplify unary functions *)
  | Exp a -> (Exp (simplify_once a))
  | Log a -> (Log (simplify_once a))
  | Sin a -> (Sin (simplify_once a))
  | Cos a -> (Cos (simplify_once a))


(* Simplifies expression by applying algebraic rules *)
let simplify expr =
  let simplified = simplify_once expr in
  if expr =:= simplified then expr
  else simplify_once simplified


(* Environment type for variable bindings *)
module StringMap = Map.Make(String)
type env = float StringMap.t


(* Create env from list of string * float pairs *)
let create_env (bindings: (string * float) list) =
  List.fold_left (fun acc (var, value) -> StringMap.add var value acc) StringMap.empty bindings


(* Update the environment with new variable values *)
let update_env (env: env) (updates: (string * float) list) =
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
  begin match simplify expr with
  | Float _ -> Float 0.
  | Var x when x = var -> Float 1.
  | Var _ -> Float 0.
  | Add (f, g) -> derivative f var +: derivative g var
  | Sub (f, g) -> derivative f var -: derivative g var
  | Pow (Var x, Float n) -> Float n *: (Var x ^: Float (n -. 1.)) (* Simplified power rule *)
  | Exp f -> Exp f *: derivative f var                 (* Chain rule with exp *)
  | Log f -> derivative f var /: f                     (* Chain rule with log *)
  | Sin f -> Cos f *: derivative f var                 (* Chain rule with sin *)
  | Cos f -> Float (-1.) *: Sin f *: derivative f var  (* Chain rule with cos *)
  | Pow (f, n) ->                                      (* General power rule *)
      let n' = n -: Float 1. in 
      let power = f ^: n' in
      n *: power *: derivative f var
  | Div (f, g) ->
      let num = (derivative f var *: g) -: (f *: derivative g var) in
      let den = g *: g in
      num /: den
  | Mult (f, g) -> (derivative f var *: g) +: (f *: derivative g var)
  end |> simplify


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
  List.map (fun var -> (var, simplify (derivative expr var))) vars


(* Computes nth derivative of an expression with respect to a variable *)
let rec nth_derivative expr var n =
  if n < 0 then invalid_arg "nth_derivative: negative order"
  else if n = 0 then expr
  else 
    let expr = simplify (derivative expr var) in 
    nth_derivative expr var (n - 1)


(* Evaluates a gradient given an environment mapping variables to values *)
let eval_grad env gradient =
  List.map (fun (var, expr) -> (var, eval env expr)) gradient  



(* GRADIENT DESCENT *)
(* Compute new values for all variables based on gradients *)
let calculate_updated_values ~expr ~env ~learning_rate =
  let updated_value (var, f') = 
    let f'_value = eval env f' in
    let current_value = StringMap.find var env in
    (var, current_value -. learning_rate *. f'_value)  (* x - lr * f'(env) *)
  in
  List.map updated_value (gradient expr)


(* Perform one iteration of gradient descent *)
let gradient_descent_step ~expr ~env ~learning_rate =
  let updates = calculate_updated_values ~expr ~env ~learning_rate in
  update_env env updates


(* Perform gradient descent to minimize the expression *)
let gradient_descent ~expr ~env ~learning_rate ~iterations =
  let rec loop env i =
    if i >= iterations then env
    else loop (gradient_descent_step ~expr ~env ~learning_rate) (i + 1)
  in
  loop env 0


(* Convert expression to a string for debugging and visualization *)
let rec string_of_expr expr =
  (* Precedence levels for operations to decide when parentheses are necessary. *)
  let precedence = function
    | Add _ | Sub _ -> 1                 (* Lowest precedence *)
    | Mult _ | Div _ -> 2                (* Medium precedence *)
    | Pow _ -> 3                         (* Higher precedence *)
    | Exp _ | Log _ | Sin _ | Cos _ -> 4 (* Functions *)
    | Float _ | Var _ -> 5               (* Constants and variables have the highest precedence *)
  in

  (* Conditionally wrap an expression in parentheses if its precedence is lower. *)
  let parenthesize parent_prec child_expr =
    let child_prec = precedence child_expr in
    let child_str = string_of_expr child_expr in
    if child_prec < parent_prec then
      "(" ^ child_str ^ ")"
    else
      child_str
  in

  (* Pattern match to construct the string representation of the expression. *)
  match expr with
  | Float x -> string_of_float x         
  | Var s   -> s                           
  | Exp a   -> "exp(" ^ string_of_expr a ^ ")"
  | Log a   -> "log(" ^ string_of_expr a ^ ")"
  | Sin a   -> "sin(" ^ string_of_expr a ^ ")"
  | Cos a   -> "cos(" ^ string_of_expr a ^ ")"
  | Add (a, b)  -> parenthesize 1 a ^ " + " ^ parenthesize 1 b
  | Sub (a, b)  -> parenthesize 1 a ^ " - " ^ parenthesize 2 b
  | Mult (a, b) -> parenthesize 2 a ^ " * " ^ parenthesize 2 b
  | Div (a, b)  -> parenthesize 2 a ^ " / " ^ parenthesize 3 b
  | Pow (a, b)  -> parenthesize 3 a ^ "^"   ^ parenthesize 4 b
