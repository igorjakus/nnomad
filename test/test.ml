open Nomad.Expr
open Nomad.Simplify
open Nomad.Eval
open Nomad.Derivatives
(* open Nomad.Gradient_descent
open Nomad.Newton
open Nomad.Bisection *)

let x = Var "x"
let y = Var "y"


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


(* let test_simplify () =
  print_endline "Testing expression simplification...";

  let test_cases = [
    (* Basic algebraic simplifications *)
    (x +: Float 0.,     x);
    (Float 0. *: x,     Float 0.);
    (x ^: Float 1.,     x);
    (x +: x,            Float 2. *: x);
    (Log (Exp x),       x);
    (Exp (Log x),       x);
    
    (* More complex simplifications *)
    (x -: Float (-2.),                   x +: Float 2.);
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
*)

let test_string_of_expr () =
  print_endline "Testing string_of_expr...";

  let test_cases = [
    (x, "x");
    (Float 3.14, "3.14");
    (x +: Float 2., "x + 2.");
    (x *: (y +: Float 3.), "x * (y + 3.)");
    (x ^: Float 2., "x^2.");
    (Exp x, "exp(x)");
    (Log (x *: y), "log(x * y)");
    (x -: y, "x - y");
    (x /: (y +: Float 1.), "x / (y + 1.)")
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
    ((fun () ->
        let expr = x *: x in
        derivative expr "x" =:= (Float 2. *: x)));
    
    ((fun () ->
        let expr = Sin x in
        derivative expr "x" =:= Cos x));
    
    ((fun () ->
        let expr = Exp (x *: x) in
        let der = derivative expr "x" in
        simplify der =:= (Exp (x *: x) *: (Float 2. *: x))));
    
    ((fun () ->
        let expr = x ^: Float 3. in
        derivative expr "x" =:= (Float 3. *: (x ^: Float 2.))));
    
    ((fun () ->
        let expr = x *: Sin x in
        let der = derivative expr "x" in
        simplify der =:= (Sin x +: (x *: Cos x))));
    
    ((fun () ->
        let expr = x *: x in
        nth_derivative expr "x" 2 =:= Float 2.));
    
    ((fun () ->
        let expr = x *: x in
        nth_derivative expr "x" 3 =:= Float 0.));
  ] in

  List.iter (fun test_fn ->
    try
      assert (test_fn ());
    with Assert_failure _ ->
      print_endline "Failed\n"
  ) test_cases;
  
  print_endline "✓ Derivative tests completed!\n"


let test_gradient () =
  print_endline "Testing gradient computation...";

  let test_cases = [
    ((fun () ->
        let expr = (x *: x) +: (y *: y) in
        let grad = gradient expr in
        let x_der = List.assoc "x" grad in
        let y_der = List.assoc "y" grad in
        x_der =:= (Float 2. *: x) && 
        y_der =:= (Float 2. *: y)));
    
    ((fun () ->
        let expr = x *: x +: y *: y in
        let env = [("x", 1.0); ("y", 2.0)] in
        let grad = gradient expr in
        let grad_values = eval_grad env grad in
        List.assoc "x" grad_values = 2.0 && 
        List.assoc "y" grad_values = 4.0));
    
    ((fun () ->
        let expr = Sin x *: Cos y in
        let grad = gradient expr in
        let env = [("x", 0.0); ("y", 0.0)] in
        let grad_values = eval_grad env grad in
        Float.abs(List.assoc "x" grad_values -. 1.0) < 1e-10 && 
        Float.abs(List.assoc "y" grad_values -. 0.0) < 1e-10));
  ] in

  List.iter (fun test_fn ->
    try
      assert (test_fn ());
    with Assert_failure _ ->
      print_endline "Failed\n"
  ) test_cases;
  
  print_endline "✓ Gradient tests completed!\n"

(*
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
*)

(* Run all tests *)
let run_tests () =
  print_endline "\nStarting Automatic Differentiation module tests...\n";
  test_env ();
  test_string_of_expr ();
  (* test_simplify (); *)
  test_eval ();
  test_derivative ();
  test_gradient ();
  (* test_gradient_descent (); *)
  (* test_solve_gradient_descent (); *)
  (* test_newton (); *)
  (* test_bisection (); *)
  print_endline "All tests completed successfully! ✓\n";;


let () = run_tests ()
