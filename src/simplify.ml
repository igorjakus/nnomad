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


(* Extract numeric coefficient and base expression,
  For example: 2x -> (2, x), 
               -x -> (-1, x) *) 
let rec factor_out_coefficient = function
  | Float c -> (c, Float 1.)
  | Neg e ->
      let (coef, base) = factor_out_coefficient e in
      (-.coef, base)
  | Product [Float c; e] ->
      (c, e)
  | Product (Float c :: es) ->
      (c, Product es)
  | e -> (1., e)


(* Combines like terms in a sorted list of expressions
   For example: [2x, 3x] -> [5x]
                [x, -x]  -> [0]
                [x, x]   -> [2x] *)
let combine_like_terms terms =
  let table = Hashtbl.create 3 in
  List.iter (fun t ->
    (* For each base, accumulate coefficient *)
    let (coef, base) = factor_out_coefficient t in
    let old_coef = try Hashtbl.find table base with Not_found -> 0. in
    Hashtbl.replace table base (old_coef +. coef)
  ) terms;

  (* Now rebuild expressions, filter out zero sums *)
  let combined =
    Hashtbl.fold (fun base coef acc ->
      if abs_float coef < 1e-12 then acc
      else if base = Float 1. then Float coef :: acc
      else if coef = 1. then base :: acc
      else if coef = -1. then Neg base :: acc
      else Product [Float coef; base] :: acc
    ) table []
  in
  sort_exprs combined


(* Combines like factors in a sorted list of expressions
   For example: [x^2, x^3] -> [x^5]
                [x, x]     -> [x^2]
                [2, 3]     -> [6] *)
let combine_like_factors terms =
  (* TODO: maybe try to do it like combine like terms? ś*)
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


(* Collection function that handles Sum and Product
  - Flattens nested sums/products
  - Sorts terms using expr_compare
  - Combines like terms / factors *)
let collect = function
  | Sum es ->
      let rec flatten_sum = function
        | Sum es :: rest -> List.append es (flatten_sum rest)
        | x :: rest -> x :: flatten_sum rest
        | [] -> []
      in
      let flattened = flatten_sum es in
      let sorted = sort_exprs flattened in
      let combined = combine_like_terms sorted in
      begin match combined with
      | [] -> Float 0.  (* Handle empty sum *)
      | [x] -> x        (* Simplify single term *)
      | xs -> Sum xs    (* Keep multiple terms *)
      end
      
  | Product es ->
      let rec flatten_product = function
        | Product es :: rest -> List.append es (flatten_product rest)
        | x :: rest -> x :: flatten_product rest
        | [] -> []
      in
      let flattened = flatten_product es in
      let sorted = sort_exprs flattened in
      let combined = combine_like_factors sorted in
      begin match combined with
      | [] -> Float 1.    (* Handle empty product *)
      | [x] -> x          (* Simplify single term *)
      | xs -> Product xs  (* Keep multiple terms *)
      end

  | _ -> failwith "improper use of collect"
  

(* Simplification function that applies rules once
  - Handles error cases (division by zero, log of non-positive)
  - Identity operations (0, 1, x, x^1, log(exp(x)), etc.)
  - Constant folding (addition, multiplication, exponentiation, etc.)
  - Trigonometric identities (sin^2(x) + cos^2(x) = 1)
  - Negation handling (negation of product, sum, power)
  - Algebraic simplifications (x + x = 2x, x * x = x^2, etc.)
  - Recursive simplification and collection *)
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

  (* Special cases for e *)
  | Pow (Exp (Float 1.), n) -> Exp n

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


  (* Trigonometric identities *)
  (* sin(π-x) = sin(x)   <=>  sin(π+x) = sin(-x)) *)
  (* | Sin (Sum (pi :: xs)) when pi =:= Constants.pi -> 
      Sin (Float (-.1.) *: Sum (xs)) *)
  
  (* sin^2(x) + cos^2(x) = 1 *)
  | Sum (Pow(Sin(x), Float 2.) :: Pow(Cos(y), Float 2.) :: xs) when x =:= y ->
      Sum ((Float 1.) :: xs) 

  (* Negation handling *)  
  | Neg (Product (x :: xs)) -> Product (Neg(x) :: xs)
  | Neg (Sum xs) -> Sum (List.map (fun e -> Neg e) xs)
  | Neg (Pow (x, Float n)) when mod_float n 2. <> 0. -> Pow (Neg x, Float n)

  (* Algebraic simplifications *)
  | Sum [x; y] when x =:= y -> 
      (Float 2. *: x)
  | Product (Pow (x, n) :: Pow (y, m) :: xs) when x =:= y -> 
      Product (Pow (x, n +: m) :: xs)
  | Product (Pow (x, n) :: y :: xs) when x =:= y ->
      Product (Pow (x, n +: Float 1.) :: xs)
  | Sum (Log x :: Log y :: xs)     -> Sum (Log (x *: y) :: xs)
  | Product (Exp x :: Exp y :: xs) -> Product (Exp (x +: y) :: xs)
  | Pow (Exp x, y) -> Exp (x *: y)
  | Pow (Pow (x, n), m) -> Pow (x, n *: m)

  (* helpful for simplifying exprs like x*y^2 / (xy)^2*)
  | Pow (Product xs, n) -> Product (List.map (fun x -> Pow (x, n)) xs)

  (* Recursive simplification and collection *)
  | Exp a -> Exp (simplify_once a)
  | Log a -> Log (simplify_once a)
  | Sin a -> Sin (simplify_once a)
  | Cos a -> Cos (simplify_once a)
  | Neg a -> Neg (simplify_once a)

  | Pow(a, b) -> Pow(simplify_once a, simplify_once b)
  
  | Sum     es -> Sum     (List.map simplify_once es) |> collect
  | Product es -> Product (List.map simplify_once es) |> collect


(* Simplification function that applies rules until no change
  - Adds safety limit to prevent infinite loops 
  - Handles error cases (division by zero, log of non-positive)
  - Identity operations (0, 1, x, x^1, log(exp(x)), etc.)
  - Constant folding (addition, multiplication, exponentiation, etc.)
  - Trigonometric identities (sin^2(x) + cos^2(x) = 1)
  - Negation handling (negation of product, sum, power)
  - Algebraic simplifications (x + x = 2x, x * x = x^2, etc.)
  - Recursive simplification and collection *)
let simplify expr =
  let rec simplify_with_limit count expr =
    if count > 100 then expr  (* Add safety limit *)
    else
      let expr' = simplify_once expr in
      if expr =:= expr' then expr
      else simplify_with_limit (count + 1) expr'
  in
  simplify_with_limit 0 expr
