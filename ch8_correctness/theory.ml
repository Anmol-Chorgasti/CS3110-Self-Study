
(* ----------------------------------SPECIFICATIONS----------------------------------------------*)
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

  (**[to_string xs] is a string of all elements in xs*)
  val to_string : ('a -> string) -> 'a t -> string
end

module ListSet : Set = struct
  (**
    List [a1;...;an] is used to implement a Set with elements {a1;...;an}
    but with all duplicates removed.
    The List itself may contain duplicates.
    The empty list represents the empty set.
  *)
  let uniq xs = List.sort_uniq Stdlib.compare xs
  type 'a t = 'a list
  let empty = []
  let size xs = xs |> uniq |> List.length (* O(n log n)*)
  let add = List.cons
  let mem = List.mem

  let union a b =
    let rec aux acc ax =
    match ax with
    | [] -> acc
    | h :: t -> aux (h::acc) t
    in
    aux (uniq b) (uniq a)

  let to_string string_of_val xs = 
    let interior =
      xs |> uniq |> List.map string_of_val |> String.concat ","
    in
    "{" ^ interior ^ "}"
end

(* ----------------------------------TESTING----------------------------------------------*)
(*
  Black box testing - tests driven by the specifications but not implementation
  1) Typical inputs
  2) Boundary cases
  3) Inputs that generate all possibile output states
  4) Inputs that raise exceptions - if exceptions are guaranteed by specification
  
  Note to self - it's a good idea to chart possible input ranges and pick along the borders
  to find weird edge cases. This will need some creativity and practice
*)

(* Practice making a test suite for an arbitrary list_max function *)
(* list_max takes in an int list and outputs an int *)
let list_max (xs : int list) = 0 (* dummy *)

(* Tests Exercise *)
(* Typical inputs *)
(* These should be normal inputs that behave as expected but with different sortings *)
let only_positive = [1;10000;1000000;0]
let only_negative = [-1000; -5; -2]
let mixed_signs = [-5;5;-6;6]

(* Boundary cases *)
(* Empty list, duplicates, *)
let empty_case = []
let dup_case = [1;1;0;0;-1;-1]
let edge_cases = [max_int + max_int; min_int + min_int; max_int; min_int]

(* GLASS BOX Testing *)
(* This form of testing can help us get to path complete test suites.
   A good flow - ? start with black box, then push the suite towards path complete using glass
   box. Note : Path complete guarantees every part of implementation is tested
   but it does NOT guarantee the implementation itself is right.
   Black Box is still needed to prove this
*)

(* PROPERTY TESTING WITH QCHECK *)
(* There seems to be a nice flow here *)
(**
  1. Use black box testing to define invariants - THIS seems to be the hard step
  2. Treat said invariants as properties and use qcheck to generate tests to CHECK them
  3. Use glass box testing to see if generated test suite achieves satisfactory coverage
*)
open QCheck2
let long_list_generator low high lb rb =
  Gen.list_size (Gen.(low -- high)) Gen.(lb -- rb)

let is_leap_year x =
  (x mod 4 = 0) && (x mod 100 <> 0 || x mod 400 = 0)

(* Invariant 1 - non multiples of 4 CANNOT be leap years *)
(**[mult4_or_non_leap_year x] is true if x mod 4 is 0 or if it is not a leap year *)
let mult4_or_non_leap_year x = x mod 4 = 0 || not (is_leap_year x)
(* Why test this way? - it forces us to confirm that is_leap_year is always false for all 
   non multiples of 4 *)

let random_non_4s = 
  Test.make
    ~name: "Non multiples of 4 cannot be leap years"
    ~count: 1000
    (Gen.(1 -- 3000))
    (mult4_or_non_leap_year)

let mult400_or_non_leap_year x = 
  x mod 400 = 0 || not (is_leap_year x)

let random_non_100s =
  Test.make
   ~name: "Multiples of 100 but not 400 cannot be leap years"
   ~count: 1000
   (Gen.(map (fun x -> x * 100) (1 -- 30)))
   mult400_or_non_leap_year


(* How is all of this working? *)
(* Visit internals of QCheck tomorow.....*)
