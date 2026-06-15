
(**
  [f x] is... - definition of function in declarative manner.
  Example : .... - an example application of function.
  Requires : .... - Preconditions
  Raises : .... - what to expect as behaviour on invalid inputs, edge cases
*)
(* Follow the above template for writing GOOD function documentation *)
(* This is usually written in a .mli file above the declaration of the val *)


(* A dummy module type created to add signatures and practice writing their specifications*)
module type dummy = sig
  (** [hd lst] is the head of [lst].
      Requires : [lst] is non empty. *)
  val hd : 'a list -> 'A

  (** [sort lst] sorts [lst] in ascending order. 
      Examples:
       [sort [3;1;2]] returns [[1;2;3]].
       [sort []] is [[]]. *)
  val sort : 'a list -> 'a list

  exception Negative

  (** [sqrt x] is the square root of [x].
      Requires : [x] be zero or positive. 
      Raises: [Negative] if x is negative. *)
  val sqrt : float -> float
end

module type Set = sig
  (**['a t] is the type of set whose elements have type ['a]*)
  type 'a t

  (**[empty] is the empty set*)
  val empty : 'a t

  (**[size x] is the number of elements in the set x. 
     Example: [size empty] is 0. *)
  val size : 'a t -> int

  (**[add x xs] is the set whose elements are x and all elements of xs.*)
  val add : 'a -> 'a t -> 'a t

  (**[mem x xs] returns true iff set xs contains x.
     [mem x empty] is false for any x. *)
  val mem : 'a -> 'a t -> bool

  (**[union a b] is a set that is the union of set a and b.
     Example: 
     Union of set with elements {1,2,3} and set with elements {3,4}
     is a set with elements {1,2,3,4}*)
  val union : 'a t -> 'a t -> 'a t
end

module ListSet : Set = struct
  (**
    List [a1;...;an] is used to implement a Set with elements {a1;...;an}
    but with all duplicates removed.
    The List itself may contain duplicates.
    The empty list represents the empty set.
  *)
  type 'a t = 'a list
  let empty = []
  let size xs = xs |> List.sort_uniq compare |> List.length (* O(n log n)*)
  let add = List.cons
  let mem = List.mem
  let union a b =
    let rec aux acc ax =
    match ax with
    | [] -> acc
    | h :: t -> aux (h::acc) t
    in
    aux (List.sort_uniq compare b) (List.sort_uniq compare a)
end