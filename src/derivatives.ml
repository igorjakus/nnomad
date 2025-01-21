open Expr
open Simplify


(* Computes the derivative of an expression with respect to a variable *)
let rec derivative var = function
  (* Base cases *)
  | Float _ -> 
      Float 0.
  | Var x when x = var -> 
      Float 1.
  | Var _ -> 
      Float 0.

  (* Basic arithmetic operations *)
  | Neg f -> 
      simplify (Neg (derivative var f))
  | Sum fs -> 
      simplify (Sum (List.map (derivative var) fs))

  (* Product rule *)
  | Product [] -> 
      Float 0.
  | Product [f] -> 
      derivative var f
  | Product (f :: fs) -> (* f * g' + f' * g *)
      simplify (f *: derivative var (Product fs) +: derivative var f *: Product fs)

  (* Power rules *)
  | Pow (Var x, Float n) when x = var -> 
      Float n *: (Var x ^: Float (n -. 1.))        (* Simple power rule *)
  | Pow (f, n) ->                                  (* General power rule *)
      let n' = n -: Float 1. in 
      let power' = f ^: n' in
      simplify (n *: power' *: derivative var f)

  (* Chain rules for elementary functions *)
  | Exp f -> 
      simplify (Exp f *: derivative var f)
  | Log f -> 
      simplify (derivative var f /: f)
  | Sin f -> 
      simplify (Cos f *: derivative var f)
  | Cos f -> 
      simplify (Float (-1.) *: Sin f *: derivative var f)


(* Compute gradient as partial derivatives with respect to all variables *)
let gradient expr: gradient =
  List.map 
    (fun var -> (var, derivative var expr)) 
    (get_variables expr)


(* Computes nth derivative of an expression with respect to a variable *)
let rec nth_derivative var n expr  =
  if n < 0 then invalid_arg "nth_derivative: negative order"
  else if n = 0 then expr
  else nth_derivative var (n - 1) (derivative var expr)
