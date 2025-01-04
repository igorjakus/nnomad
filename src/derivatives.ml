open Expr

(* Computes the derivative of an expression with respect to a variable *)
let rec derivative expr var =
  match expr with
  (* Base cases *)
  | Float _ -> 
      Float 0.
  | Var x when x = var -> 
      Float 1.
  | Var _ -> 
      Float 0.

  (* Basic arithmetic operations *)
  | Add (f, g) -> 
      simplify (derivative f var +: derivative g var)
  | Sub (f, g) -> 
      simplify (derivative f var -: derivative g var)
  | Mult (f, g) -> 
      simplify ((derivative f var *: g) +: (f *: derivative g var))
  | Div (f, g) ->
      let num = (derivative f var *: g) -: (f *: derivative g var) in
      let den = g *: g in
      simplify (num /: den)

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

  (* Preserve laziness in derivatives *)
  | Lazy f -> lazy_expr (fun () -> derivative (f ()) var)


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
