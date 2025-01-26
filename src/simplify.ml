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
  (* TODO: don't use Hashtbl *)
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
  in

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
  (* TODO: Improve readability *)
  let factor_out_power = function
    | Float c -> (c, Float 1., Float 0.)
    | Pow (b, e) -> (1., b, e)
    | Neg (Pow (b, e)) -> (-1., b, e)
    | Neg b -> (-1., b, Float 1.)
    | b -> (1., b, Float 1.)
  in

  (* Merge two exponent expressions into a single Sum or Float *)
  let merge_exp e1 e2 =
    match e1, e2 with
    | Float f1, Float f2 -> Float (f1 +. f2)
    | Float f1, _        -> Sum [Float f1; e2]
    | _, Float f2        -> Sum [e1; Float f2]
    | _                  -> Sum [e1; e2]
  in

  (* Update exponent for a given base in the association list *)
  let rec update base exp = function
    | [] ->
        (* If numeric exponent is effectively zero, skip it *)
        begin match exp with
         | Float f when abs_float f < 1e-12 -> []
         | _ -> [base, exp]
        end 
    | (k, old_exp) :: xs ->
        if k =:= base then
          let new_exp = merge_exp old_exp exp in
          match new_exp with
          | Float f when abs_float f < 1e-12 -> xs
          | _ -> (k, new_exp) :: xs
        else
          (k, old_exp) :: update base exp xs
  in

  (* Fold over the terms to build (numeric factor, exponents map) *)
  let (num_acc, assoc) =
    List.fold_left (fun (acc_coef, assoc) t ->
      let (coef, base, exp) = factor_out_power t in
      let new_coef =
        let tmp = acc_coef *. coef in
        if abs_float tmp < 1e-12 then 0. else tmp
      in
      let assoc' = update base exp assoc in
      (new_coef, assoc')
    ) (1., []) terms
  in

  (* If numeric factor ~ 0, entire product is 0 *)
  if abs_float num_acc < 1e-12 then
    [Float 0.]
  else
    (* Rebuild the product from the association list *)
    let factors =
      List.fold_left (fun acc (base, exp) ->
        match exp with
        | Float f when abs_float f < 1e-12 -> acc
        | Float 0. -> acc
        | Float 1. -> base :: acc
        | _ -> Pow (base, exp) :: acc
      ) [] assoc
    in
    let final =
      if abs_float (num_acc -. 1.) < 1e-12 then factors
      else if abs_float (num_acc +. 1.) < 1e-12 then [Neg (Product factors)]
      else Float num_acc :: factors
    in
    sort_exprs final


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
