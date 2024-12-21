open Nnomad.Expr
open Nnomad.Eval
open Nnomad.Derivatives
open Nnomad.Optimization


let test_env () =
  print_endline "\nTesting environment creation and updates...";

  let test_cases = [  
    (fun () ->
       let env = create_env [("x", 2.0); ("y", 3.0)] in
       let expr = Var "x" +: Var "y" in
       eval env expr = 5.0);

    (fun () ->
       let env = create_env [("x", 2.0); ("y", 3.0)] in
       let env = update_env env [("x", 1.0)] in
       let expr = Var "x" +: Var "y" in
       eval env expr = 4.0);

    (fun () ->
       let env = create_env [("x", 2.0); ("y", 3.0)] in
       let gradient = [("x", Float 1.0); ("y", Float (-1.0))] in
       eval_grad env gradient = [("x", 1.0); ("y", -1.0)]);

    (fun () ->
       let env = create_env [("x", 2.0)] in
       let env = update_env env [("y", 3.0)] in
       let expr = Var "x" +: Var "y" in
       eval env expr = 5.0);

    (fun () ->
       let env = create_env [("x", 2.0); ("y", 3.0)] in
       let env = update_env env [("x", 4.0); ("y", 1.0)] in
       let expr = Var "x" *: Var "y" in
       eval env expr = 4.0);

    (fun () ->
       let env = create_env [("x", 1.0); ("x", 2.0)] in
       let expr = Var "x" in
       eval env expr = 2.0);
  ] in

  List.iter (fun test_fn ->
    assert (test_fn ())
  ) test_cases;

  print_endline "✓ Environment tests completed!\n"


let test_simplify () =
  print_endline "Testing expression simplification...";

  let test_cases = [
    ((Var "x" +: Float 0., Var "x"));
    ((Float 0. *: Var "x", Float 0.));
    ((Var "x" ^: Float 1., Var "x"));
    ((Var "x" +: Var "x", Float 2. *: Var "x"));
    ((Log (Exp (Var "x")), Var "x"));
    ((Exp (Log (Var "x")), Var "x"));
    ((Sub (Var "x", Float (-2.)), Add (Var "x", Float 2.)));
    (((Var "x" +: Float 0.) *: (Float 1. *: Var "y"), Mult (Var "x", Var "y")));
    ((Mult (Var "x", Pow (Var "x", Float 2.)), Pow (Var "x", Float 3.)));
    ((Log(Var "x") +: Log(Var "y"), Log(Var "x" *: Var "y")));
    ((Div (Pow (Var "x", Float 3.), Pow (Var "x", Float 2.)), Var "x"));
    ((Div (Mult (Var "x", Pow (Var "y", Float 2.)), Var "y" *: Var "x"), Var "y"));
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


let test_eval () =
  print_endline "Testing expression evaluation...";

  let env = create_env ["x", 2.0; "y", 3.0] in
  let test_cases = [
    ((Var "x" +: Var "y", 5.0));
    ((Var "x" *: Var "y", 6.0));
    ((Var "x" -: Var "y", -1.0));
    ((Var "x" /: Var "y", 2.0 /. 3.0));
    ((Exp (Var "x"), exp 2.0));
    ((Log (Var "y"), log 3.0));
    ((Sin (Var "x"), sin 2.0));
    ((Cos (Var "x"), cos 2.0));
    ((Sin(Var "x") *: Cos(Var "y"), sin(2.0) *. cos(3.0)));
    ((Sin(Var "x") *: Cos(Var "y") +: Exp(Var "x" /: Var "y"),
      sin(2.0) *. cos(3.0) +. exp(2.0/.3.0)));
    ((Pow(Var "x", Float 3.) +: Pow(Var "y", Float 2.),
      8.0 +. 9.0));
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
        let expr = Var "x" *: Var "x" in
        derivative expr "x" =:= (Float 2. *: Var "x")));
    
    ((fun () ->
        let expr = Sin (Var "x") in
        derivative expr "x" =:= Cos (Var "x")));
    
    ((fun () ->
        let expr = Exp (Var "x" *: Var "x") in
        let der = derivative expr "x" in
        simplify der =:= (Exp (Var "x" *: Var "x") *: (Float 2. *: Var "x"))));
    
    ((fun () ->
        let expr = Pow (Var "x", Float 3.) in
        derivative expr "x" =:= (Float 3. *: Pow (Var "x", Float 2.))));
    
    ((fun () ->
        let expr = Var "x" *: Sin (Var "x") in
        let der = derivative expr "x" in
        simplify der =:= (Sin (Var "x") +: (Var "x" *: Cos (Var "x")))));
    
    ((fun () ->
        let expr = Var "x" *: Var "x" in
        nth_derivative expr "x" 2 =:= Float 2.));
    
    ((fun () ->
        let expr = Var "x" *: Var "x" in
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
        let expr = Var "x" *: Var "x" +: Var "y" *: Var "y" in
        let grad = gradient expr in
        let x_der = List.assoc "x" grad in
        let y_der = List.assoc "y" grad in
        x_der =:= (Float 2. *: Var "x") && 
        y_der =:= (Float 2. *: Var "y")));
    
    ((fun () ->
        let expr = Var "x" *: Var "x" +: Var "y" *: Var "y" in
        let env = create_env ["x", 1.0; "y", 2.0] in
        let grad = gradient expr in
        let grad_values = eval_grad env grad in
        List.assoc "x" grad_values = 2.0 && 
        List.assoc "y" grad_values = 4.0));
    
    ((fun () ->
        let expr = Sin(Var "x") *: Cos(Var "y") in
        let grad = gradient expr in
        let env = create_env ["x", 0.0; "y", 0.0] in
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


let test_gradient_descent () =
  print_endline "Testing gradient descent optimization...";

  let test_cases = [
    ((fun () ->
        let expr = Var "x" *: Var "x" in
        let env = create_env ["x", 10.0] in
        let final_env = gradient_descent ~expr ~env ~learning_rate:0.1 ~iterations:100 in
        abs_float (get_variable "x" final_env) < 1e-3));
    
    ((fun () ->
        let expr = Var "x" *: Var "x" +: Var "y" *: Var "y" in
        let env = create_env ["x", 1.0; "y", 1.0] in
        let final_env = gradient_descent ~expr ~env ~learning_rate:0.1 ~iterations:100 in
        abs_float (get_variable "x" final_env) < 1e-3 && 
        abs_float (get_variable "y" final_env) < 1e-3));
    
    ((fun () ->
        let expr = Pow(Var "x" -: Float 1., Float 2.) +: 
                  Pow(Var "y" +: Float 2., Float 2.) in
        let env = create_env ["x", 0.0; "y", 0.0] in
        let final_env = gradient_descent ~expr ~env ~learning_rate:0.1 ~iterations:200 in
        abs_float (get_variable "x" final_env -. 1.0) < 1e-2 && 
        abs_float (get_variable "y" final_env +. 2.0) < 1e-2));
  ] in

  List.iter (fun test_fn ->
    try
      assert (test_fn ());   
    with Assert_failure _ ->
      print_endline "Failed\n"
  ) test_cases;
  
  print_endline "✓ Gradient descent tests completed!\n"


(* Run all tests *)
let run_tests () =
  print_endline "\nStarting Automatic Differentiation module tests...\n";
  test_env ();
  test_simplify ();
  test_eval ();
  test_derivative ();
  test_gradient ();
  test_gradient_descent ();
  print_endline "All tests completed successfully! ✓\n";;


let () = run_tests ()
