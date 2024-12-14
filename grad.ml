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
  (* TODO: 1: zminimalizuj liczbe nawiasow, zeby zachowac jednoznacznosc reprezentacji *)
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
  (* TODO: 2: make some order and enough comments but not too much, 
  also ensure that it is always simplified maximally, 
  like Log (expr) -> Log (simplify expr) but it can be sometimes simplified further,
  by simplify Log (simplify expr), but we also want to ensure recursion ends *)
  
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

  (* Sort expressions *)
  | Mult (x, Float y) -> Mult (Float y, simplify x)  (* prefer constant before other expr in multiplicaton *)
  
  (* Unique function properties *)
  | Log (Exp x) -> simplify x
  | Exp (Log x) -> simplify x
  | Sub (x, Float y) when y < 0. -> Add (x, Float (-.y))
  | Add (x, y) when simplify x = simplify y -> Mult(Float 2., simplify x)
  | Sub (x, y) when simplify x = simplify y -> Float 0.
  | Mult(Pow (x, n), y) when x = y -> simplify (Pow(x, (n +: Float 1.)))
  | Mult(y, Pow(x, n)) when x = y -> simplify (Pow(x, (n +: Float 1.)))

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
  (* TODO: 2 maybe more base cases would be helpful for doing TODO: 2 *)
  | Float x -> Float x
  | Var x -> Var x


(* TODO: 3 simplify pojawia sie mnostwo razy w funkcjach ponizej, czy da sie jakos tak zrobic,
zeby nie powielac kodu? przy kazdej operacji chcmemy uzyc simplify wiec moze jakos to opakujemy w monade? 
nie wime czy ma to sens *)


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
(* TODO: 4: uzywanie simplify wyglada troche brzydko, moze da sie cos z tym zrobic? *)
let rec derivative expr var = 
  let f' =
    match simplify expr with
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
  in 
  simplify f'


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



(* TESTS *)
(* TODO: add more tests, especially to gradient descent and simplify! *)
let test_env () =
  print_endline "Testing environment creation and updates...";
  let env = create_env [("x", 2.0); ("y", 3.0)] in
  let expr = Var "x" +: Var "y" in
  assert (eval env expr = 5.0);
  let env = update_env env [("x", 1.0)] in
  assert (eval env expr = 4.0);
  
  (* Additional tests *)
  let env = create_env [] in
  assert (StringMap.is_empty env);
  
  let env = create_env [("x", 1.0)] in
  assert (StringMap.cardinal env = 1);
  
  print_endline "✓ Environment tests passed successfully!\n"

let test_simplify () =
  print_endline "Testing expression simplification...";
  (* Basic simplification *)
  assert (simplify (Var "x" +: Float 0.) = Var "x");
  assert (simplify (Float 0. *: Var "x") = Float 0.);
  assert (simplify (Var "x" ^: Float 1.) = Var "x");

  (* Advanced simplification *)
  assert (simplify (Log (Exp (Var "x"))) = Var "x");
  assert (simplify (Exp (Log (Var "x"))) = Var "x");
  assert (simplify (Sub (Var "x", Float (-2.))) = Add (Var "x", Float 2.));
  
  (* Nested expressions *)
  let expr = (Var "x" +: Float 0.) *: (Float 1. *: Var "y") in
  assert (simplify expr = Mult (Var "x", Var "y"));
  
  print_endline "✓ Simplification tests passed successfully!\n"

let test_eval () =
  print_endline "Testing expression evaluation...";
  let env = create_env ["x", 2.0; "y", 3.0] in

  (* Basic arithmetic *)
  assert (eval env (Var "x" +: Var "y") = 5.0);
  assert (eval env (Var "x" *: Var "y") = 6.0);
  assert (eval env (Var "x" -: Var "y") = -1.0);
  assert (Float.abs(eval env (Var "x" /: Var "y") -. (2.0 /. 3.0)) < 1e-10);

  (* Advanced functions *)
  assert (Float.abs(eval env (Exp (Var "x")) -. exp 2.0) < 1e-10);
  assert (Float.abs(eval env (Log (Var "y")) -. log 3.0) < 1e-10);
  assert (Float.abs(eval env (Sin (Var "x")) -. sin 2.0) < 1e-10);
  assert (Float.abs(eval env (Cos (Var "x")) -. cos 2.0) < 1e-10);

  (* Complex expressions *)
  let complex_expr = Sin(Var "x") *: Cos(Var "y") +: Exp(Var "x" /: Var "y") in
  assert (Float.abs(eval env complex_expr -. (sin(2.0) *. cos(3.0) +. exp(2.0/.3.0))) < 1e-10);

  print_endline "✓ Evaluation tests passed successfully!\n"

let test_derivative () =
  print_endline "Testing derivative computation...";
  (* Basic derivatives *)
  let expr = Var "x" *: Var "x" in
  assert (derivative expr "x" = (Float 2. *: Var "x"));

  let expr = Sin (Var "x") in
  assert (derivative expr "x" = Cos (Var "x"));

  (* Chain rule *)
  let expr = Exp (Var "x" *: Var "x") in
  let der = derivative expr "x" in
  assert (simplify der = Exp (Var "x" *: Var "x") *: (Float 2. *: Var "x"));

  (* Higher-order derivatives *)
  let expr = Var "x" *: Var "x" in
  assert (nth_derivative expr "x" 2 = Float 2.);
  assert (nth_derivative expr "x" 3 = Float 0.);

  (* Test error handling *)
  try 
    let _ = nth_derivative expr "x" (-1) in
    failwith "Expected invalid_arg exception"
  with Invalid_argument _ -> ();

  print_endline "✓ Derivative tests passed successfully!\n"

let test_gradient () =
  print_endline "Testing gradient computation...";
  (* Simple quadratic function *)
  let expr = Var "x" *: Var "x" +: Var "y" *: Var "y" in
  let grad = gradient expr in

  assert (List.assoc "x" grad = Float 2. *: Var "x");
  assert (List.assoc "y" grad = Float 2. *: Var "y");

  (* Test gradient evaluation *)
  let env = create_env ["x", 1.0; "y", 2.0] in
  let grad_values = eval_grad env grad in

  assert (List.assoc "x" grad_values = 2.0);
  assert (List.assoc "y" grad_values = 4.0);

  (* More complex function *)
  let expr = Sin(Var "x") *: Cos(Var "y") in
  let grad = gradient expr in
  let env = create_env ["x", 0.0; "y", 0.0] in
  let grad_values = eval_grad env grad in
  
  assert (Float.abs(List.assoc "x" grad_values -. 1.0) < 1e-10);
  assert (Float.abs(List.assoc "y" grad_values -. 0.0) < 1e-10);

  print_endline "✓ Gradient tests passed successfully!\n"

let test_gradient_descent () =
  print_endline "Testing gradient descent optimization...";
  (* Test with quadratic function *)
  let expr = Var "x" *: Var "x" in
  let env = create_env ["x", 10.0] in
  let learning_rate = 0.1 in
  let iterations = 100 in

  let final_env = gradient_descent ~expr ~env ~learning_rate ~iterations in
  let final_value = StringMap.find "x" final_env in

  (* The minimum of x^2 is at x = 0 *)
  assert (abs_float final_value < 1e-3);

  (* Test with more complex function *)
  let expr = Var "x" *: Var "x" +: Var "y" *: Var "y" in
  let env = create_env ["x", 1.0; "y", 1.0] in
  let final_env = gradient_descent ~expr ~env ~learning_rate ~iterations in
  
  assert (abs_float (StringMap.find "x" final_env) < 1e-3);
  assert (abs_float (StringMap.find "y" final_env) < 1e-3);

  print_endline "✓ Gradient descent tests passed successfully!\n"

(* Run all tests *)
let run_tests () =
  print_endline "\nStarting Automatic Differentiation module tests...\n";
  test_env ();
  test_simplify ();
  test_eval ();
  test_derivative ();
  test_gradient ();
  test_gradient_descent ();
  print_endline "All tests completed successfully! ✓\n"
