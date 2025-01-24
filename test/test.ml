open Nomad.Expr
open Nomad.Simplify
open Nomad.Eval
open Nomad.Derivatives
open Nomad.Gradient_descent
open Nomad.Newton
open Nomad.Bisection


(* Shortcuts for variables *)
let x = Var "x"
let y = Var "y"
let z = Var "z"


let test_env () =
  print_endline "\nTesting environment creation and updates...";

  let test_cases = [  
    (fun () ->
       let env = [("x", 2.0); ("y", 3.0)] in
       let expr = x +: y in
       eval env expr = 5.0);

    (fun () ->
       let env = [("x", 2.0); ("y", 3.0)] in
       let env = update_env env [("x", 1.0)] in
       let expr = x +: y in
       eval env expr = 4.0);

    (fun () ->
       let env = [("x", 2.0); ("y", 3.0)] in
       let gradient = [("x", Float 1.0); ("y", Float (-1.0))] in
       eval_grad env gradient = [("x", 1.0); ("y", -1.0)]);

    (fun () ->
       let env = [("x", 2.0)] in
       let env = update_env env [("y", 3.0)] in
       let expr = x +: y in
       eval env expr = 5.0);

    (fun () ->
       let env = [("x", 2.0); ("y", 3.0)] in
       let env = update_env env [("x", 4.0); ("y", 1.0)] in
       let expr = x *: y in
       eval env expr = 4.0);
  ] in

  List.iter (fun test_fn ->
    assert (test_fn ())
  ) test_cases;

  print_endline "✓ Environment tests completed!\n"


let test_simplify () =
  print_endline "Testing expression simplification...";

  let test_cases = [
    (* Basic algebraic simplifications *)
    (x +: Float 0.,     x);
    (Float 0. *: x,     Float 0.);
    (x ^: Float 1.,     x);
    (x +: x,            Float 2. *: x);
    (Log (Exp x),       x);
    (Exp (Log x),       x);
    (Sum [Neg (x); Neg (y); x; y; z], z);
    (Sum [x; y; z; Neg (x); Neg (y)], z);
    
    (* More complex simplifications *)
    (x -: Float (-2.),                   Float 2. +: x);
    ((x +: Float 0.) *: (Float 1. *: y), x *: y);
    (x *: (x ^: Float 2.),               x ^: Float 3.);
    (Log x +: Log y,                     Log (x *: y));
    ((x ^: Float 3.) /: (x ^: Float 2.), x);
    ((x *: (y ^: Float 2.)) /: (y *: x), y);

    (* Negation handling *)
    (Neg (Neg x),       x);
    (Neg (x +: y),      Neg x -: y);
    
    (* Trigonometric identities *)
    ((Sin x ^: Float 2.) +: (Cos x ^: Float 2.), Float 1.);
    ((Sin y ^: Float 2.) +: (Cos y ^: Float 2.), Float 1.);
    (* (Sin (pi -: x), Sin x); *) (* sin(π-x) = sin(x) *)

    (* Division simplifications *)
    (y /: (x /: y), (Pow(y, Float 2.)) /: x);
    (x /: (y /: x), (Pow(x, Float 2.)) /: y);
    ((x /: y) /: (y /: x), (Pow(x, Float 2.)) /: (y *: y));
    
    (* Mixed operations *)
    ((x /: y) *: (y /: x), Float 1.);
    ((x *: y) /: (y *: x), Float 1.);
    
    (* Combined simplifications *)
    (((x +: y) /: x) -: Float 1., y /: x);
    ((x *: y) /: (x *: (y +: Float 1.)), y /: (y +: Float 1.));

    (* Subtraction simplifications *)
    (Sin x -: Sin x, Float 0.);

    (* Constants simplifications *)
    (Exp (Float 1.) ^: x, Exp x);
  ] in

  List.iter (fun (input, expected) ->
    let result = simplify input in
    try
      assert (result =:= expected);   
    with Assert_failure _ ->
      Printf.printf "Input: %s\nExpected: %s\nGot: %s\n\n"
        (string_of_expr input)
        (string_of_expr expected)
        (string_of_expr result)
  ) test_cases;
  
  print_endline "✓ Simplification tests completed!\n"


let test_string_of_expr () =
  print_endline "Testing string_of_expr...";

  let test_cases = [
    (* Variables *)
    (x, "x");
    (Var "monad", "monad");

    (* Constants *)
    (Float 0., "0");
    (Float 1., "1");
    (Float 3.1415, "3.1415");
    (Float (-2.13), "-2.13");
    (pi, "π");
    (e, "e");

    (* Products *)
    (Product [x; y; z], "x * y * z");
    (Product [x; y; z; Float 2.], "x * y * z * 2");
    (Product [x; y; z; Float (-2.13)], "x * y * z * (-2.13)");
    (Product [x; y; z; Float (-2.13); Float 3.], "x * y * z * (-2.13) * 3");
    (Product [x; y +: z; x], "x * (y + z) * x");

    (* Negation *)
    (Neg x, "-x");
    (Neg (Neg x), "-(-x)");
    (Neg (x +: y), "-(x + y)");
    (Neg (x *: y), "-(x * y)");
    (Neg (x /: y), "-(x / y)");
    (Neg (x ^: y), "-(x^y)");
    (Neg (Exp x), "-exp(x)");
    (Neg (Log x), "-log(x)");
    (Neg (Sin x), "-sin(x)");
    (Neg (Cos x), "-cos(x)");
    (Neg (Sin x *: Cos y), "-(sin(x) * cos(y))");
    (Neg (Sin x *: Cos y +: Exp(x /: y)), "-(sin(x) * cos(y) + exp(x / y))");
    (Neg (Sum [x; y; z]), "-(x + y + z)");
    (Neg (Product [x; y; z]), "-(x * y * z)");

    (* Sums *)
    (x +: Float 2., "x + 2");
    (x +: Float 3.5, "x + 3.5");
    (x +: Neg(Float 3.5), "x - 3.5");
    (x +: y, "x + y");
    (x -: y, "x - y");
    (Sum [x; y; z], "x + y + z");
    (Sum [Neg (x); Neg (y); x; y; z], "-x - y + x + y + z");
    (Sum [x; y; z; Neg (x); Neg (y)], "x + y + z - x - y");

    (* Powers *)
    (x ^: Float 2., "x^2");
    (x ^: Float 3.13, "x^3.13");
    (x ^: Float (-2.13), "x^{-2.13}");
    (x ^: y, "x^y");
    (x ^: (Float 2. *: x), "x^{2 * x}");
    (x ^: (y +: Float 2.), "x^{y + 2}");
    (x ^: (y *: z), "x^{y * z}");
    (x ^: (y /: z), "x^{y / z}");
    (x ^: (y ^: z), "x^{y^z}");
    (x ^: (Neg y), "x^{-y}");

    (* Exponential, logarithm and trigonometric *)
    (Exp x, "exp(x)");
    (Log x, "log(x)");
    (Log (x *: y), "log(x * y)");
    (Sin x, "sin(x)");
    (Cos x, "cos(x)");

    (* Division *)
    (x /: y, "x / y");
    (x /: Neg(y), "x / (-y)");
    (Float 1. /: x, "1 / x");
    (x /: Float 2., "x / 2");
    (x /: Float 2.5, "x / 2.5");
    (x /: Float (-2.5), "x / -2.5");
    (x /: (y +: Float 2.), "x / (y + 2)");
    (y /: (x /: y), "y / (x / y)");
    ((x /: y) /: (y /: x), "(x / y) / (y / x)"); (* FIXME: *)
    ((x *: y) /: (y *: x), "(x * y) / (y * x)"); (* FIXME: *)
    ((x +: y) /: x, "(x + y) / x");

    (* Mixed expressions *)
    (x *: (y +: Float 3.), "x * (y + 3)");
    (x *: (y +: Float 3.5), "x * (y + 3.5)");
    (x *: (y +: Float (-3.5)), "x * (y - 3.5)");

  ] in

  List.iter (fun (input, expected) ->
    let result = string_of_expr input in
    if result <> expected then
      Printf.printf "Input: %s\nExpected: %s\nGot: %s\n\n"
        (string_of_expr input) expected result
  ) test_cases;

  print_endline "✓ string_of_expr tests completed!\n"


let test_eval () =
  print_endline "Testing expression evaluation...";

  let env = [("x", 2.0); ("y", 3.0)] in
  let test_cases = [
    (x +: y, 5.0);
    (x *: y, 6.0);
    (x -: y, -1.0);
    (x /: y, 2.0 /. 3.0);
    (Exp x, exp 2.0);
    (Log y, log 3.0);
    (Sin x, sin 2.0);
    (Cos x, cos 2.0);
    (Sin x *: Cos y, sin(2.0) *. cos(3.0));
    (Sin x *: Cos y +: Exp(x /: y),
      sin(2.0) *. cos(3.0) +. exp(2.0/.3.0));
    ((x ^: Float 3.) +: (y ^: Float 2.),
      8.0 +. 9.0);
  ] in

  List.iter (fun (expr, expected) ->
    let result = eval env expr in
    try
      assert (Float.abs(result -. expected) < 1e-10); 
    with Assert_failure _ ->
      Printf.printf "Expected: %f\nGot: %f\n\n" expected result
  ) test_cases;
  
  print_endline "✓ Evaluation tests completed!\n"


let test_derivative () =
  print_endline "Testing derivative computation...";

  let test_cases = [
    (* Basic derivatives *)
    (x *: x, "x", Float 2. *: x);
    (Sin x, "x", Cos x);
    (Exp (x *: x), "x", Exp (x *: x) *: (Float 2. *: x));
    (x ^: Float 3., "x", Float 3. *: (x ^: Float 2.));
    (x *: Sin x, "x", Sin x +: (x *: Cos x));
    ((x +: y) *: Sin x, "x", (x +: y) *: Cos x +: Sin x);
    (Log x, "x", Float 1. /: x);

    (* Zero derivatives (with respect to x) *)
    (y ^: Float 3., "x", Float 0.);
    (Sin y, "x", Float 0.);
    (Exp y, "x", Float 0.);
    (y *: y, "x", Float 0.);

    (* Mixed expressions *)
    (x *: (y ^: Float 2.), "x", y ^: Float 2.);
    ((x +: y) ^: Float 3., "x", Float 3. *: ((x +: y) ^: Float 2.));
    (Sin (x *: y), "x", y *: Cos (x *: y));

    (* Complex expressions *)
    (Exp (x *: y) *: Sin (x +: y), "x", 
     (y *: Exp (x *: y) *: Sin (x +: y)) +: (Exp (x *: y) *: Cos (x +: y)));

    (* Nested expressions *)
    ((x ^: Float 2.) *: Sin (y ^: Float 2.), "x", 
     Float 2. *: x *: Sin (y ^: Float 2.));

    (* Logarithmic expressions *)
    (Log (x *: y), "x", Float 1. /: x);
    (Log (x ^: y), "x", y /: x);

    (* Quotient rule cases *)
    ((x ^: Float 2.) /: y, "x", Float 2. *: x /: y);
    (x /: (y ^: Float 2.), "x", Float 1. /: (y ^: Float 2.));
  ] in

  List.iter (fun (input, var, expected) ->
    let result = derivative var input in
    try
      assert (simplify result =:= simplify expected);   
    with Assert_failure _ ->
      Printf.printf "Input: %s\nVariable: %s\nExpected: %s\nGot: %s\n\n"
        (string_of_expr input)
        var
        (string_of_expr expected)
        (string_of_expr result)
  ) test_cases;
  
  print_endline "✓ Derivative tests completed!\n"


let test_nth_derivative () =
  print_endline "Testing nth derivative computation...";

  let test_cases = [
    (x *: x, "x", 2, Float 2.);
    (x *: x, "x", 3, Float 0.);
    (x ^: Float 3., "x", 2, Float 6. *: x);
    (x ^: Float 3., "x", 3, Float 6.);
    (x ^: Float 3., "x", 4, Float 0.);
    (Sin x, "x", 4, Sin x);
    (Cos x, "x", 4, Cos x);
  ] in

  List.iter (fun (input, var, n, expected) ->
    let result = nth_derivative var n input in
    try
      assert (simplify result =:= simplify expected);   
    with Assert_failure _ ->
      Printf.printf "Input: %s\nVariable: %s\nN: %d\nExpected: %s\nGot: %s\n\n"
        (string_of_expr input)
        var
        n
        (string_of_expr expected)
        (string_of_expr result)
  ) test_cases;
  
  print_endline "✓ Nth derivative tests completed!\n"


let test_eval_grad () =
  print_endline "Testing gradient evaluation...";

  let test_cases = [
    ([("x", 1.0); ("y", 2.0)],
     [("x", Float 2.); ("y", Float 4.)],
     [("x", 2.0); ("y", 4.0)]);
    
    ([("x", 0.0); ("y", 0.0)],
     [("x", Cos x); ("y", Neg (Sin x))],
     [("x", 1.0); ("y", 0.0)]);
    
    ([("x", 2.0); ("y", 3.0)],
     [("x", Float 1.0); ("y", Float (-1.0))],
     [("x", 1.0); ("y", -1.0)]);
    
    ([("x", 1.0); ("y", 1.0)],
     [("x", x); ("y", y)],
     [("x", 1.0); ("y", 1.0)]);
  ] in

  List.iter (fun (env, grad, expected) ->
    let result = eval_grad env grad in
    try
      assert (result = expected);
    with Assert_failure _ ->
      Printf.printf "Env: %s\nGrad: %s\nExpected: %s\nGot: %s\n\n"
        (String.concat ", " (List.map (fun (v,n) -> v ^ "=" ^ string_of_float n) env))
        (String.concat ", " (List.map (fun (v,e) -> v ^ "=" ^ string_of_expr e) grad))
        (String.concat ", " (List.map (fun (v,n) -> v ^ "=" ^ string_of_float n) expected))
        (String.concat ", " (List.map (fun (v,n) -> v ^ "=" ^ string_of_float n) result))
  ) test_cases;
  
  print_endline "✓ Gradient evaluation tests completed!\n"


let test_gradient () =
  print_endline "Testing gradient computation...";

  let test_cases = [
    (* Basic expressions *)
    (x *: x, [("x", Float 2. *: x)]);
    (x +: y, [("x", Float 1.); ("y", Float 1.)]);
    ((x *: x) +: (y *: y), [("x", Float 2. *: x); ("y", Float 2. *: y)]);
    
    (* Trigonometric expressions *)
    (Sin x *: Cos y, [("x", Cos x *: Cos y); ("y", Neg(Sin x) *: Sin y)]);
    (Sin (x *: y), [("x", y *: Cos (x *: y)); ("y", x *: Cos (x *: y))]);
    
    (* Exponential and logarithmic *)
    (Exp (x +: y), [("x", Exp (x +: y)); ("y", Exp (x +: y))]);
    (Log (x *: y), [("x", Float 1. /: x); ("y", Float 1. /: y)]);
    
    (* Complex expressions *)
    ((x ^: Float 2.) *: Sin y, 
     [("x", Float 2. *: x *: Sin y); 
      ("y", (x ^: Float 2.) *: Cos y)]);
    
    (Exp (x *: y) +: Log (x +: y),
     [("x", y *: Exp (x *: y) +: Float 1. /: (x +: y));
      ("y", x *: Exp (x *: y) +: Float 1. /: (x +: y))]);
  ] in

  List.iter (fun (expr, expected) ->
    let result = gradient expr in
    try
      List.iter2 
        (fun (var1, expr1) (var2, expr2) ->
           assert (var1 = var2 && simplify expr1 =:= simplify expr2))
        (List.sort compare result)
        (List.sort compare expected)
    with Assert_failure _ ->
      Printf.printf "Expression: %s\nExpected gradient: %s\nGot: %s\n\n"
        (string_of_expr expr)
        (String.concat ", " (List.map (fun (v,e) -> v ^ "=" ^ string_of_expr e) expected))
        (String.concat ", " (List.map (fun (v,e) -> v ^ "=" ^ string_of_expr e) result))
  ) test_cases;
  
  print_endline "✓ Gradient computation tests completed!\n"


let test_gradient_descent () =
  print_endline "Testing gradient descent optimization...";

  let test_cases = [
    ((fun () ->
        let expr = x *: x in
        let env = [("x", 10.0)] in
        match gradient_descent ~expr ~env ~learning_rate:0.1 ~max_iter:100 with
        | Ok final_env -> abs_float (get_value "x" final_env) < 1e-3
        | Error _ -> false));
    
    ((fun () ->
        let expr = x *: x +: y *: y in
        let env = [("x", 1.0); ("y", 1.0)] in
        match gradient_descent ~expr ~env ~learning_rate:0.1 ~max_iter:100 with
        | Ok final_env -> 
            abs_float (get_value "x" final_env) < 1e-3 && 
            abs_float (get_value "y" final_env) < 1e-3
        | Error _ -> false));
    
    ((fun () ->
        let expr = Pow(x -: Float 1., Float 2.) +: 
                  Pow(y +: Float 2., Float 2.) in
        let env = [("x", 0.0); ("y", 0.0)] in
        match gradient_descent ~expr ~env ~learning_rate:0.1 ~max_iter:200 with
        | Ok final_env ->
            abs_float (get_value "x" final_env -. 1.0) < 1e-2 && 
            abs_float (get_value "y" final_env +. 2.0) < 1e-2
        | Error _ -> false));
  ] in

  List.iter (fun test_fn ->
    try
      assert (test_fn ());   
    with Assert_failure _ ->
      print_endline "Failed\n"
  ) test_cases;
  
  print_endline "✓ Gradient descent tests completed!\n"


let test_solve_gradient_descent () =
  print_endline "Testing solve_gradient_descent...";

  let test_cases = [
    ((fun () ->
        let eq = (x *: x, Float 1.) in
        let env = [("x", 0.1)] in
        match solve_gradient_descent eq ~initial_guess:env ~learning_rate:0.1 ~max_iter:100 with
        | Ok final_env -> abs_float (abs_float (get_value "x" final_env) -. 1.) < 0.1
        | Error _ -> false));
    
    ((fun () ->
        let eq = (x *: x +: y *: y, Float 1.) in
        let env = [("x", 1.0); ("y", 1.0)] in
        match solve_gradient_descent eq ~initial_guess:env ~learning_rate:0.1 ~max_iter:1000 with
        | Ok final_env -> abs_float (eval final_env (fst eq -: snd eq)) < 0.1
        | Error _ -> false));

    ((fun () ->
        let eq = (Pow(x -: Float 1., Float 2.) +: Pow(y +: Float 2., Float 2.), Float 0.) in
        let env = [("x", 0.1); ("y", -0.2)] in
        match solve_gradient_descent eq ~initial_guess:env ~learning_rate:0.1 ~max_iter:1000 with
        | Ok final_env -> abs_float (eval final_env (fst eq -: snd eq)) < 0.01
        | Error _ -> false));
  ] in

  List.iter (fun test_fn ->
    try
      assert (test_fn ());   
    with Assert_failure _ ->
      print_endline "Failed\n"
  ) test_cases;
  
  print_endline "✓ solve_gradient_descent tests completed!\n"


let test_newton () =
  print_endline "Testing Newton-Raphson solver...";

  let test_cases = [
    (* Basic equations *)
    ((fun () ->
       let eq = (x ^: Float 2., Float 4.) in
       match solve_newton eq ~initial_guess:3.0 ~max_iter:50 with
       | Ok x -> Float.abs (x -. 2.0) < 1e-6
       | Error _ -> false));

    (* Trigonometric equations *)
    ((fun () ->
       let eq = (Cos x, Float 0.) in
       match solve_newton eq ~initial_guess:1.0 ~max_iter:50 with
       | Ok x -> Float.abs (x -. (Float.pi /. 2.)) < 1e-6
       | Error _ -> false));

    ((fun () ->
       let eq = (Sin x, Float 0.5) in
       match solve_newton eq ~initial_guess:0.0 ~max_iter:50 with
       | Ok x -> Float.abs (x -. (Float.pi /. 6.)) < 1e-6
       | Error _ -> false));

    (* Complex trigonometric equations *)
    ((fun () ->
       let eq = (Sin x *: Cos x, Float 0.25) in
       match solve_newton eq ~initial_guess:0.0 ~max_iter:50 with
       | Ok x -> Float.abs (x -. 0.261799387791) < 1e-6
       | Error _ -> false));

    (* Exponential and logarithmic equations *)
    ((fun () ->
       let eq = (Exp x, Float 1.) in
       match solve_newton eq ~initial_guess:1.0 ~max_iter:50 with
       | Ok x -> Float.abs x < 1e-6
       | Error _ -> false));

    ((fun () ->
       let eq = (Log x, Float 1.) in
       match solve_newton eq ~initial_guess:2.0 ~max_iter:50 with
       | Ok x -> Float.abs (x -. exp 1.) < 1e-6
       | Error _ -> false));

    (* Polynomial equations *)
    ((fun () ->
       let eq = (((x -: Float 2.13) ^: Float 3.) +: (x -: Float 2.13), Float 0.) in
       match solve_newton eq ~initial_guess:2.0 ~max_iter:50 with
       | Ok x -> Float.abs (x -. 2.13) < 1e-6
       | Error _ -> false));

    (* Multivariate equations *)
    ((fun () ->
       let eq = (x *: x +: y *: y, Float 1.) in
       let env_guess = [("x", 0.5); ("y", 0.5)] in
       match solve_newton_multivar eq ~initial_guess:env_guess ~max_iter:100 with
       | Ok final_env ->
           let val_x = get_value "x" final_env in
           let val_y = get_value "y" final_env in
           Float.abs (val_x *. val_x +. val_y *. val_y -. 1.0) < 1e-6
       | Error _ -> false));

    ((fun () ->
       let eq = (x *: y, Float 1.) in
       let env_guess = [("x", 2.05); ("y", 0.45)] in
       match solve_newton_multivar eq ~initial_guess:env_guess ~max_iter:100 with
       | Ok final_env ->
           let x = get_value "x" final_env in
           let y = get_value "y" final_env in
           Float.abs (x *. y -. 1.0) < 1e-6
       | Error _ -> false));

    ((fun () ->
       let eq = (Sin x *: Cos y, Float 0.25) in
       let env_guess = ["x", 0.2; "y", 0.2] in
       match solve_newton_multivar eq ~initial_guess:env_guess ~max_iter:100 with
       | Ok final_env ->
           let x = get_value "x" final_env in
           let y = get_value "y" final_env in
           Float.abs (sin(x) *. cos(y) -. 0.25) < 1e-6
       | Error _ -> false));
  ] in

  List.iter (fun test_fn ->
    try
      assert (test_fn ());
    with Assert_failure _ ->
      print_endline "Failed\n"
  ) test_cases;
  
  print_endline "✓ Newton-Raphson solver tests completed!\n"


let test_bisection () =
  print_endline "Testing Bisection method...";

  let test_cases = [
    (* Basic polynomial *)
    ((fun () ->
       let expr = Pow (x, Float 2.) -: Float 4. in
       match bisection ~f:expr ~a:0.0 ~b:3.0 ~tolerance:1e-6 ~max_iter:100 with
       | Ok result -> Float.abs (result -. 2.0) < 1e-6
       | Error _ -> false));

    (* Trigonometric equation *)
    ((fun () ->
       let expr = Cos x -: Float 0.5 in
       match bisection ~f:expr ~a:0.0 ~b:2.0 ~tolerance:1e-6 ~max_iter:100 with
       | Ok result -> Float.abs (result -. 1.0472) < 1e-3
       | Error _ -> false));

    (* Mixed equation *)
    ((fun () ->
       let expr = Sin(x) -: (x /: Float 2.) in
       match bisection ~f:expr ~a:1.0 ~b:2.0 ~tolerance:1e-6 ~max_iter:100 with
       | Ok result -> Float.abs (result -. 1.8954) < 1e-3
       | Error _ -> false));
  ] in

  List.iter (fun test_fn ->
    try
      assert (test_fn ());
    with Assert_failure _ -> 
      print_endline "Failed\n"
  ) test_cases;
  
  print_endline "✓ Bisection method tests completed!\n"


(* Run all tests *)
let run_tests () =
  print_endline "\nStarting Automatic Differentiation module tests...\n";
  test_env ();
  test_string_of_expr ();
  test_simplify ();
  test_eval ();
  test_derivative ();
  test_nth_derivative ();
  test_eval_grad ();    (* Changed order *)
  test_gradient ();     (* Changed order *)
  test_gradient_descent ();
  test_solve_gradient_descent ();
  test_newton ();
  test_bisection ();
  print_endline "All tests completed successfully! ✓\n";;


let () = run_tests ()
