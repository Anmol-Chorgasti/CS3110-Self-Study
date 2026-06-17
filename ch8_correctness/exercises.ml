
(* SOLUTIONS FOR SOME CHAPTER 8 EXERCISES *)

(*-------------------------------------------------------------------------------------*)
(* 1. Poly Spec *)

(** [Poly] represents immutable polynomials with integer coefficients.
    For explanation purposes, polynomials are represented as $ax^n + ... + z$ *)
module type Poly = sig
  (** [t] is the type of polynomials. *)
  type t

  (** [term] is a single term in a polynomial *)
  type term

  exception Negpower

  (** [make_term a b] is ax^b
      Requires: b to be 0 or positive.
      Raises [Negpower] if b < 0.
      Example: 
      [make_term 5 0] is 5.
      [make_term 4 2] is 4x^2 *)
  val make_term : int -> int -> term

  (** [create tl] is polynomial p of tl terms.
      Requires: each term in p to have positive or 0 x power. Raises [Negpower] otherwise.
      Each term in polynomial p has unique power of x.
      Example: If tl has both 5x^2 and 4x^2 as terms, 9x^2 is in p
      More Examples:
       Multiple terms : [create [5x^2; 4x; 1]] is $5x^2 + 4x + 1$
       Single term : [create [5x^2]] is $5x^2$
       No term : [create []] is $0$ *)
  val create : term list -> t

  (** [combine xa xb] is the sum of polynomials xa and xb.
      Examples:
      Typical case : [combine $5x^2 + 1$ $4x$] is $5x^2 + 4x + 1$
      Typical case : [combine $5x^2$ $4x^2$] is $9x^2$
      Edge case : [combine $4x$ $0$] is $4x$*)
  val combine : t -> t -> t

  (** [to_string p] is p in string format.
      String representation has terms organized in descending order of x powers.
      Example :
      if [p] has terms 5x^2,4x,1 then [to_string p] is 5x^2 + 4x + 1. 4x + 5x^2 + 1 is invalid*)
  val to_string : t -> string

  (** [eval x p] is [p] evaluated at [x]. Example: if [p] represents
      $3x^3 + x^2 + x$, then [eval 10 p] is [3110]. *)
  val eval : int -> t -> int

  (** [extract b p] is the coefficient of x^b term in p.
      Requires : b is 0 or positive. 
      Raises [Negpower] if b < 0. 
      If x^b not in polynomial, 0 is returned. *)
  val extract : int -> t -> int
end

(*-------------------------------------------------------------------------------------*)
(* 2. Poly Implementation *)
module Poly : Poly = struct
  (**
  A Term is (Power b, Coeff a) which is ax^b in the polynomial.
  A polynomial is represented as a list of such terms. 
  The list cannot contain terms with (Power 0, Coeff 0) in it.
  The list cannot contain multiple terms with the same powers. It must be "merged" at all times
  The list must always be in descending order of powers.
  Negpower is raised if a list of terms contains any term with negative power
  Example of a valid list : [(Power 3, Coeff 2);(Power 1, Coeff 0); (Power 0; Coeff 0)]
  *)

  type powx = Power of int
  type coeffx = Coeff of int

  type term = powx * coeffx
  type t = term list

  exception Negpower

  let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

  let make_term a b =
    if (b < 0) then raise Negpower
    else (Power b, Coeff a)
  
  let compare_terms a b =
    match (a, b) with
    | (Power x, Coeff _), (Power y, Coeff _) ->
      if x < y then (-1)
      else if x = y then 0
      else 1
    
  let merge tl = 
    let rec aux acc_p acc_c = function
    | [] -> (
      match acc_c with
      | (Power _, Coeff y) ->
        if y = 0 then acc_p else acc_c :: acc_p
      )
    | (Power b, Coeff a) :: t -> 
      (
      match acc_c with
      | (Power x, Coeff y) ->
        if x = b then aux acc_p ((Power b, Coeff (y+a))) t
        else (
          if y = 0 then aux acc_p ((Power b, Coeff a)) t
          else aux (acc_c::acc_p) ((Power b, Coeff a)) t
      ))
      in
      tl |> List.sort compare_terms |> aux [] ((Power 0, Coeff 0))

  let is_list_positive tl =
    let is_good_term (x:term) =
    match x with
    | (Power b, Coeff _) -> b >= 0
    in
    List.fold_left (fun y x -> (is_good_term x) && y) true tl
  
  let create tl =
    if is_list_positive tl then merge tl
    else raise Negpower
      
  let combine p1 p2 =
    let rec aux acc = function
    | [] -> merge acc
    | h :: t -> aux (h::acc) t
  in aux p2 p1

  let eval x p =
    let eval_term = function
      | (Power a, Coeff b) -> (pow x a) * b
  in
  p |> List.map eval_term |> List.fold_left ( + ) 0

  let rec extract b = 
  if b < 0 then raise Negpower
  else
  function
  | [] -> 0
  | (Power x, Coeff a) :: t -> if x=b then a else extract b t

  let to_string p =
    let term_string = function
    | (Power b, Coeff a) ->
      if (a > 0) then string_of_int a ^ "x" ^ "^" ^ string_of_int b
      else "(" ^ string_of_int a ^ ")" ^ "x" ^ "^" ^ string_of_int b
    in
    let rec aux acc = function
    | []-> if String.length acc = 0 then "0" else acc
    | h :: [] -> 
      if String.length acc = 0 then term_string h else acc ^ " + " ^ term_string h
    | h :: t -> 
      let acc_start = 
        if String.length acc = 0 then term_string h else acc ^ " + " ^ term_string h
      in aux acc_start t
  in aux "" p
end

