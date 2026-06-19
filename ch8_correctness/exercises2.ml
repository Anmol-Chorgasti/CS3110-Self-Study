
(* 3. Interval Arithmetic *)
(** [Interval] is an immutable set of integers in a range. 
    Example : [[4,8]] is the set {4,5,6,7,8}.
    For representation purposes an Interval from a to b is shown as [a,b]. *)
module type Interval = sig

  (**[t] is the type of an Interval*)
  type t

  exception Empty 
  exception ZeroDivisionError
  exception Undefined

  (**[make a b] is the interval [[a,b]] or [[b,a]].
     if b < a then [[b,a]] else [[a,b]].*)
  val make : int -> int -> t

  (**[intersection i1 i2] is [[max(a,c), min(b,d)]] iff b >= c and a <= d. 
     Raises [Empty] otherwise.*)
  val intersection : t -> t -> t

  (**[union i1 i2] is [[min(a,c), max(b,d)]] iff b >= c and a <= d.
     Raises [Empty] otherwise. *)
  val union : t -> t -> t

  (**[equal i1 i2] is True iff i1 is [[a,b]] and i2 is [[c,d]] and a = c and b = d.
     False otherwise*)
  val equal : t -> t -> bool

  (**[compare i1 i2] is 1 iff i1 > i2. 0 iff i1 = i2. -1 if i1 < i2. 
     If i1 is [[a,b]] and i2 is [[c,d]] then 
      1) i1 < i2 iff b < c.
      2) i1 > i2 iff a > d.
      3) i1 = i2 iff a = c and b = d.
      4) Undefined otherwise. Raises [Undefined] in this case.*)
  val compare : t -> t -> int

  (**[width i1] is b-a iff i1 is [[a,b]]*)
  val width : t -> int

  (**[abs_val i1] is max(|a|, |b|)*)
  val abs_val : t -> int

  (**[add i1 i2] is [[a + c, b + d]] iff i1 is [[a,b]] and i2 is [[c,d]]*)
  val add : t -> t -> t

  (**[subtract i1 i2] is [[a - c, b - d]] iff i1 is [[a,b]] and i2 is [[c,d]]*)
  val subtract : t -> t -> t

  (**[mult i1 i2] is [[min (ac,ad,bc,bd), max (ac,ad,bc,bd)]] 
     iff i1 is [[a,b]] and i2 is [[c,d]] *)
  val mult : t -> t -> t

  (**[divide i1 i2] is [[min (a/c,a/d,b/c,b/d), max (a/c,a/d,b/c,b/d)]] 
     iff i1 is [[a,b]] and i2 is [[c,d]].
     Requires : c is not 0 and d is not 0. Raises [ZeroDivisionError] otherwise *)
  val divide : t -> t -> t

  (**[to_string i1] is string form of i1. 
     If i1 is from a to b, then string form is $[[a,b]]$. *)
  val to_string : t -> string
end

module Interval : Interval = struct
  (**An [Interval] is represented as a pair of subtypes - (Left a, Right b)
     A valid interval must have a <= b in the above representation. *)
  
  type leftt = Left of int
  type rightt = Right of int
  type t = leftt * rightt

  exception Empty 
  exception ZeroDivisionError
  exception Undefined

  let make a b =
    if a <= b then (Left a, Right b) else (Left b, Right a)

  let is_intersect_true i1 i2 =
     match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) -> (b >= c && a <= d)

  let intersection i1 i2 =
    match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) ->
      if is_intersect_true i1 i2 then make (max a c) (min b d)
      else raise Empty
  
  let union i1 i2 =
    match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) ->
      if is_intersect_true i1 i2 then make (min a c) (max b d)
      else raise Empty

  let equal i1 i2 =
     match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) -> (a = c) && (b = d)

  let compare i1 i2 =
     match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) ->
      if a > d then 1
      else if b < c then -1
      else if (equal i1 i2) then 0
      else raise Undefined

  let width = function
  | (Left a, Right b) -> b-a

  let abs_val = function
  | (Left a, Right b) -> max (abs a) (abs b)

  let add i1 i2 = 
    match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) -> make (a + c) (b + d)

  let subtract i1 i2 = 
    match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) -> make (a - d) (b - c)

  
  let mult i1 i2 =
    match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) -> (
      let a_new = List.fold_left min (a*c) [a*c; a*d; b*c; b*d] in
      let b_new = List.fold_left max (a*c) [a*c; a*d; b*c; b*d] in
      make a_new b_new
    )

  let divide i1 i2 =
    match (i1, i2) with
    | (Left a, Right b), (Left c, Right d) -> (
      if c = 0 || d = 0 then raise ZeroDivisionError
      else (
        let a_new = List.fold_left min (a/c) [a/c; a/d; b/c; b/d] in
        let b_new = List.fold_left max (a/c) [a/c; a/d; b/c; b/d] in
        make a_new b_new
      )
    )
  
  let to_string i1 =
    match i1 with
    | (Left a, Right b) -> "[" ^ string_of_int a ^ "," ^ string_of_int b ^ "]"
end


(*4. Function Maps*)
module FuncMap = struct
  (**AF: Client treats data structures created using this module as having type ('k, 'v) t.
         'k is type of keys and 'v is type of value. If a map and a key value is provided,
         either value is returned to the user or an Empty exception is raised if key is not
         in map.
    Representation type: The module utilizes functions to represent above AF.
    The RT is 'k -> 'v.  *)
  type ('k, 'v) t = 'k -> 'v
  exception Empty
  let empty = fun _ -> raise Empty
  let add k v mp = fun key -> if key=k then v else mp key
  let find k mp = mp k
  let remove k mp = fun key -> if key=k then raise Empty else mp key
  let mem k mp =
    try 
    match mp k with
    | _ -> true
    with
    | Empty -> false  
end