open Expr
open Simplify


(* Computes the derivative of an expression with respect to a variable *)
let rec derivative expr var = (* TODO: swap order of arguments *)
  match expr with
  (* Base cases *)
  | Float _ -> 
      Float 0.
  | Var x when x = var -> 
      Float 1.
  | Var _ -> 
      Float 0.

  (* Basic arithmetic operations *)
  | Neg f -> 
      simplify (Neg (derivative f var))
  | Sum fs -> 
      simplify (Sum (List.map (fun e -> derivative e var) fs))
  | Product fs -> 
      let fs' = List.mapi (fun i e -> 
        let rest = List.filter (fun _ -> true) fs in
        let rest' = List.mapi (fun j f -> 
          if i = j then derivative e var else f
        ) rest in
        Product rest'
      ) fs in
      simplify (Sum fs')

  (* Power rules *)
  | Pow (Var x, Float n) -> 
      Float n *: (Var x ^: Float (n -. 1.))        (* Simple power rule *)
  | Pow (f, n) ->                                  (* General power rule *)
      let n' = n -: Float 1. in 
      let power = f ^: n' in
      simplify (n *: power *: derivative f var)

  (* Chain rules for elementary functions *)
  | Exp f -> 
      simplify (Exp f *: derivative f var)
  | Log f -> 
      simplify (derivative f var /: f)
  | Sin f -> 
      simplify (Cos f *: derivative f var)
  | Cos f -> 
      simplify (Float (-1.) *: Sin f *: derivative f var)


(* Compute gradient as partial derivatives with respect to all variables *)
let gradient expr: gradient =
  List.map 
    (fun var -> (var, derivative expr var)) 
    (get_variables expr)


(* Computes nth derivative of an expression with respect to a variable *)
let rec nth_derivative expr var n =
  if n < 0 then invalid_arg "nth_derivative: negative order"
  else if n = 0 then expr
  else nth_derivative (derivative expr var) var (n - 1)
