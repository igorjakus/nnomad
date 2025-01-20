open Expr


(* Checks if expression contains division by zero
   Returns true if expression contains:
   - negative power of 0 *)
let rec check_zero_division = function
  | Pow (Float 0., Float n) when n < 0. -> true
  | Neg e | Sin e | Cos e | Exp e | Log e -> check_zero_division e
  | Sum es | Product es -> List.exists check_zero_division es
  | Pow (a, b) -> check_zero_division a || check_zero_division b
  | _ -> false


(* Combines like terms in a sorted list of expressions
   For example: [2x, 3x] -> [5x]
                [x, -x]  -> [0]
                [x, x]   -> [2x] *)
let combine_like_terms terms =
  let rec combine = function
    | [] -> []
    | [x] -> [x]
    | t1 :: t2 :: rest ->
        begin match t1, t2 with
        (* Handle constant combinations *)
        | Float n1, Float n2 -> 
            Float (n1 +. n2) :: combine rest
        | Neg (Float n1), Float n2 -> 
            Float (n2 -. n1) :: combine rest
        | Float n1, Neg (Float n2) -> 
            Float (n1 -. n2) :: combine rest
            
        (* Handle cancellations and combinations *)
        | x, y when x =:= y -> 
            Product [Float 2.; x] :: combine rest
        | x, Neg y when x =:= y -> 
            Float 0. :: combine rest
        | Neg x, y when x =:= y -> 
            Float 0. :: combine rest
        | x, Neg y -> 
            x :: Neg y :: combine rest  (* Keep negation explicit *)
        | x, y -> 
            x :: combine ( y :: rest)
        end
  in
  combine terms


(* Combines like factors in a sorted list of expressions
   For example: [x^2, x^3] -> [x^5]
                [x, x]     -> [x^2]
                [2, 3]     -> [6] *)
let combine_like_factors terms =
  let rec combine = function
    | [] -> []
    | [x] -> [x]
    | Float 0. :: _ -> [Float 0.]
    | t1 :: t2 :: rest ->
        begin match t1, t2 with
        (* Handle constant combinations *)
        | Float n1, Float n2 -> 
            Float (n1 *. n2) :: combine rest
            
        (* Handle power combinations *)
        | Pow (x1, n1), Pow (x2, n2) when x1 =:= x2 ->
            Pow (x1, Sum [n1; n2]) :: combine rest
        | x, Pow (y, n) when x =:= y ->
            Pow (x, Sum [Float 1.; n]) :: combine rest
        | x, y when x =:= y ->
            Pow (x, Float 2.) :: combine rest
            
        (* Keep first term and continue *)
        | x, _ -> x :: combine (t2 :: rest)
        end 
  in
  combine terms


(* Main collection function that handles Sum and Product
   - Sorts terms using expr_compare
   - Combines like terms *)
let collect = function
  | Sum es ->
      let rec flatten_sum = function
        | Sum [x] :: rest -> x :: flatten_sum rest
        | Sum (x :: xs) :: rest -> x :: flatten_sum (Sum xs :: rest)
        | x :: rest -> x :: flatten_sum rest
        | [] -> []
      in
      let flattened = flatten_sum es in
      let sorted = List.sort expr_compare flattened in
      Sum (combine_like_terms sorted)
      
  | Product es ->
      let rec flatten_product = function
        | Product [x] :: rest -> x :: flatten_product rest
        | Product (x :: xs) :: rest -> x :: flatten_product (Product xs :: rest)
        | x :: rest -> x :: flatten_product rest
        | [] -> []
      in
      let flattened = flatten_product es in
      let sorted = List.sort expr_compare flattened in
      Product (combine_like_factors sorted)

  | _ -> failwith "improper use of collect"


(* Helper function to apply simplify recursively only once *)
let rec simplify_once expr = 
  (* Error handling *)
  match expr with
  | _ when check_zero_division expr -> 
      failwith "division by zero"
  | Log (Float x) when x <= 0. -> 
      failwith "log of non-positive number"
    
  (* Identity operations *)
  | Float _ | Var _           -> expr
  | Sum (Float 0. :: xs)      -> Sum xs
  | Product (Float 1. :: xs)  -> Product xs
  | Pow (x, Float 1.)         -> x
  | Log (Exp x) | Exp (Log x) -> x
  | Neg (Neg x)               -> x
  | Sum     (x :: [])         -> x
  | Product (x :: [])         -> x

  (* Constant folding *)
  | Sum     (Float a :: Float b :: xs) -> Sum     (Float (a +. b) :: xs) 
  | Product (Float a :: Float b :: xs) -> Product (Float (a *. b) :: xs)
  
  | Pow (Float a, Float b) -> Float (a ** b)
  | Log (Float x)  -> Float (log x)
  | Exp (Float x)  -> Float (exp x)
  | Sin (Float x)  -> Float (sin x)
  | Cos (Float x)  -> Float (cos x)
  | Neg (Float x)  -> Float (-.x)
  | Pow (_, Float 0.) -> Float 1.
  | Product (Float 0. :: _) -> Float 0.
  | Sum     [] -> Float 0.
  | Product [] -> Float 1.

  (* Negation handling *)  
  | Neg (Product (x :: xs)) -> Product (Neg(x) :: xs)
  | Neg (Sum xs) -> Sum (List.map (fun e -> Neg e) xs)
  | Neg (Pow (x, Float n)) when mod_float n 2. <> 0. -> Pow (Neg x, Float n)

  (* Algebraic simplifications *)
  | Sum [x; y] when x =:= y -> (Float 2. *: x)
  | Product [Pow (x, n); y] when x =:= y -> Pow (x, n +: Float 1.)
  | Sum (Log x :: Log y :: xs) -> Sum (Log (x *: y) :: xs)

  (* Recursive simplification and collection *)
  | Exp a -> Exp (simplify_once a)
  | Log a -> Log (simplify_once a)
  | Sin a -> Sin (simplify_once a)
  | Cos a -> Cos (simplify_once a)
  | Neg a -> Neg (simplify_once a)

  | Pow(a, b) -> Pow(simplify_once a, simplify_once b)
  
  | Sum     es -> Sum     (List.map simplify_once es) |> collect
  | Product es -> Product (List.map simplify_once es) |> collect


(* Main simplification function that applies rules until no changes *)
let simplify expr =
  let rec simplify_with_limit count expr =
    if count > 100 then expr  (* Add safety limit *)
    else
      let expr' = simplify_once expr in
      if expr =:= expr' then expr
      else simplify_with_limit (count + 1) expr'
  in
  simplify_with_limit 0 expr
