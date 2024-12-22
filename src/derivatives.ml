open Expr


(* Computes the derivative of an expression with respect to a variable *)
let rec derivative expr var = 
  begin match simplify expr with
  | Float _ -> Float 0.
  | Var x when x = var -> Float 1.
  | Var _ -> Float 0.
  | Add (f, g) -> derivative f var +: derivative g var
  | Sub (f, g) -> derivative f var -: derivative g var
  | Pow (Var x, Float n) -> 
    Float n *: (Var x ^: Float (n -. 1.))              (* Simplified power rule *)
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
let gradient expr: gradient =
  List.map 
  (fun var -> (var, derivative expr var)) 
  (get_variables expr)


(* Computes nth derivative of an expression with respect to a variable *)
let rec nth_derivative expr var n =
  if n < 0 then invalid_arg "nth_derivative: negative order"
  else if n = 0 then expr
  else 
    let expr = simplify (derivative expr var) in 
    nth_derivative expr var (n - 1)
