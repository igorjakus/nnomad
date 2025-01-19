(* Core expression type *)
type expr =
  | Float of float
  | Var of string
  | Neg of expr
  | Sum of expr list        (* n-ary addition *)
  | Product of expr list    (* n-ary multiplication *)
  | Pow of expr * expr      (* exponentation *)
  | Exp of expr             (* exponential function *)
  | Log of expr             (* natural logarithm *)
  | Sin of expr
  | Cos of expr


(* Gradient is a list of pairs (var, partial derivative with respect to var) *)
type gradient = (string * expr) list


(* Equation is lhs expression and rhs expression *)
type equation = expr * expr


(* Operator overloading for more natural expression syntax *)
let ( +: ) a b = Sum [a; b]
let ( -: ) a b = Sum [a; Neg b]
let ( *: ) a b = Product [a; b]
let ( /: ) a b = Product [a; Pow (b, Float (-1.))]
let ( ^: ) x n = Pow (x, n)

let rec (=:=) e1 e2 =
  match e1, e2 with
  | Float a, Float b -> abs_float (a -. b) < 1e-10
  | Var a,   Var b   -> a = b
  | Neg a,   Neg b   -> a =:= b

  | Sum     a1, Sum     a2 
  | Product a1, Product a2 -> List.equal (=:=) a1 a2

  | Pow (a1, b1), Pow (a2, b2) -> a1 =:= a2 && b1 =:= b2

  | Exp a1, Exp a2 
  | Log a1, Log a2
  | Sin a1, Sin a2 
  | Cos a1, Cos a2 -> a1 =:= a2

  | _ -> false


  let rec expr_compare e1 e2 =
    let rec expr_category = function
      | Float _   -> 0  (* Constants first *)
      | Var _     -> 2  (* Then variables *)
      | Product _ -> 4  (* Then products *)
      | Sum _     -> 6  (* Then sums *)
      | Pow _     -> 8  (* Then powers *)
      | Sin _     -> 10  (* Then trig functions *)
      | Cos _     -> 12
      | Log _     -> 14  (* Then other functions *)
      | Exp _     -> 16
      | Neg e     -> expr_category e - 1  (* Negation in between *)
      (* TODO: shouldn't pow be in between? Maybe all the functions? *)
    in
  
    let cat1 = expr_category e1 in
    let cat2 = expr_category e2 in
    if cat1 <> cat2 then compare cat1 cat2
    else match e1, e2 with
      (* Built-in comparison *)
      | Float a, Float b -> compare a b
      | Var a, Var b -> String.compare a b
      
      (* Sum and product comparison *)
      | Sum es1, Sum es2 
      | Product es1, Product es2 ->
        let sorted1 = List.sort expr_compare es1 in
        let sorted2 = List.sort expr_compare es2 in
        List.compare expr_compare sorted1 sorted2
      
      (* Pow comparison *)
      | Pow (a1, b1), Pow (a2, b2) -> 
        let cmp = expr_compare a1 a2 in
        if cmp <> 0 then cmp else expr_compare b1 b2
        
      (* Unary functions *)
      | Neg a, Neg b 
      | Sin a, Sin b
      | Cos a, Cos b
      | Log a, Log b
      | Exp a, Exp b -> expr_compare a b
      
      | _ -> failwith "Invalid comparison"


(* Set of strings *)
module VarSet = Set.Make (struct
  type t = string
  let compare = compare
end)

(* Function to get list of variables in expression *)
let get_variables expr =
  let rec aux set expr =
    match expr with
    | Float _ -> set
    | Var x -> VarSet.add x set
    | Neg e -> aux set e
    | Exp e | Log e | Sin e | Cos e -> aux set e
    | Sum es | Product es -> List.fold_left aux set es
    | Pow (e1, e2) -> aux (aux set e2) e1
  in
  VarSet.elements (aux VarSet.empty expr)


(* Get variable if there's exactly one, fail if 0 or >= 2 *)
let get_variable expr = 
  match get_variables expr with 
  | []  -> failwith "no variables in expression"
  | [x] -> x
  | _   -> failwith "more than one variable in expression"


(* General precedence function for determining when parentheses are needed. *)
let precedence = function
  | Sum _ -> 1                         (* Lowest precedence *)
  | Product _ -> 2                     (* Medium precedence *)
  | Pow _ -> 3                         (* Higher precedence *)
  | Exp _ | Log _ | Sin _ | Cos _ -> 4 (* Functions *)
  | Float _ | Var _ -> 5               (* Constants and variables have the highest precedence *)
  | Neg _ -> 6                         (* Negation has the highest precedence *)


(* General parenthesize function for different conversions. *)
let parenthesize parent_prec child_expr converter =
  let child_prec = precedence child_expr in
  let child_str = converter child_expr in
  if child_prec < parent_prec then
    "(" ^ child_str ^ ")"
  else
    child_str


(* Convert expression to a string for debugging and visualization *)
let rec string_of_expr expr =
  match expr with
  | Float x -> string_of_float x
  | Var s -> s

  | Neg (Float n) -> string_of_float (-.n)
  | Neg (Sum es) -> 
      "- " ^ String.concat " - " (List.map (fun e -> parenthesize 1 e string_of_expr) es)
  | Sum es -> 
      (match es with
       | [] -> "0"
       | first :: rest ->
           string_of_expr first ^ 
           String.concat "" 
             (List.map (fun e -> match e with
               | Neg x -> " - " ^ parenthesize 1 x string_of_expr
               | x -> " + " ^ string_of_expr x) rest))
  | Product es -> 
      (match es with
       | [] -> "1"
       | [x] -> string_of_expr x
       | first :: rest ->
           parenthesize 2 first string_of_expr ^ 
           String.concat "" 
             (List.map (fun e -> match e with
               | Pow (x, Float n) when n < 0. -> 
                   " / " ^ parenthesize 2 x string_of_expr
               | x -> " * " ^ parenthesize 2 x string_of_expr) rest))
  | Neg (Product _ as x) -> 
      "-" ^ parenthesize 6 x string_of_expr
  | Neg x -> "-" ^ parenthesize 6 x string_of_expr
  | Exp a -> "exp(" ^ string_of_expr a ^ ")"
  | Log a -> "log(" ^ string_of_expr a ^ ")"
  | Sin a -> "sin(" ^ string_of_expr a ^ ")"
  | Cos a -> "cos(" ^ string_of_expr a ^ ")"
  | Pow (a, Float n) when n < 0. ->
      parenthesize 2 a string_of_expr ^ "^" ^ string_of_float n ^ "."
  | Pow (a, b) -> 
      parenthesize 3 a string_of_expr ^ "^" ^ parenthesize 4 b string_of_expr
