
(* HASH TABLES - What are they? How to implement a Map ADT using it? *)

module type Map = sig

  (**[('k,'v) t] is the type of maps that binds 
     ['k] type keys to ['v] type values. *)
  type ('k,'v) t

  (**[empty] is the empty map. *)
  val empty : ('k, 'v) t

  (**[insert k v mp] is a new map with all key value bindings 
     from [mp] and an additional binding between [k] and [v]. 
     If [mp] already had a binding to [k].
     The binding is replaced by binding [k] to [v] in the new map.*)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (**[find key mp] is [Some v] if [v] is bound to [key] in [mp]. 
     Returns [None] otherwise. *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (**[remove key mp] is the same map as mp with the binding 
     to [key] removed if [key] was in [mp].
     If [key] was not in [mp]. The map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

  (** [of_list lst] is a map containing the same bindings as
      association list [lst].
      Requires: [lst] does not contain any duplicate keys. *)
  val of_list : ('k * 'v) list -> ('k, 'v) t

  (** [bindings m] is an association list containing the same
      bindings as [m]. There are no duplicates in the list. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module ListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list
  let empty = []
  let insert k v mp = (k,v)::mp (*O(1)*)
  let find = List.assoc_opt (*O(n)*)
  let remove k mp = List.filter (fun (key,_) -> key <> k) mp (*O(n)*)
  let of_list lst = lst
  let bindings mp = mp |> List.sort_uniq (fun (k1,_) (k2,_) -> Stdlib.compare  k1 k2) (*O(nlogn)*)
end

(* Creating a more restricted Map using a Direct Address Table *)

module type DirectAddressMap = sig

  (**['v t] is the type of maps that binds 
     integer keys to ['v] type values. *)
  type 'v t

  (**[empty n] is the empty direct address table with keys from 0 to n-1.
     The default value for each key is the None option type *)
  val empty : int -> 'v t

  (**[capacity mp] is number of integer keys in [mp]*)
  val capacity : 'v t -> int

  (**[insert k v mp] mutates mp by inserting value v into index k.
     Requires k to be between 0 and capacity-1.*)
  val insert : int -> 'v -> 'v t -> unit

  (**[find key mp] is [Some v] if [key] is valid index in [mp]. 
     Returns [None] otherwise. *)
  val find : int -> 'v t -> 'v option

  (**[remove key mp] mutates mp by deleting the value at index [key]
     IFF [key] is a valid index. 
     Otherwise [mp] is unchanged. *)
  val remove : int -> 'v t -> unit

  (** [of_list lst] is a pair of direct address tables containing the same bindings as
      association list [lst].
      The First map a.k.a Kmap is a map between integer indices and key names.
      Second map a.k.a Vmap is a map between integer indices and values bound to key names stored 
      in same index in Kmap.
      A key value pair can be accessed as (Kmap val at index i, Vmap val at index i)
      Requires: [lst] does not contain any duplicate keys. *)
  val of_list : ('k * 'v) list -> 'k t * 'v t

  (** [bindings m] is an association list containing the same
      bindings as [m]. There are no duplicates in the list. *)
  val bindings :  'v t -> (int * 'v) list
end

module DirectAddressMap = struct
  type 'v t = 'v option array
  let capacity dat = Array.length dat

  let empty n = Array.make n None

  let is_index_valid i dat = i >= 0 && i < capacity dat

  let insert k v dat = if is_index_valid k dat then dat.(k) <- Some v else ()

  let remove k dat = if is_index_valid k dat then dat.(k) <- None else ()

  let find k dat = if is_index_valid k dat then dat.(k) else None

  let of_list lst = 
    ( lst |> List.map (fun (k,_) -> Some k) |> Array.of_list,
      lst |> List.map (fun (_,v) -> Some v) |> Array.of_list )

  let bindings dat =
    dat
    |> Array.to_list
    |> List.mapi (fun i opt -> (i,opt))
    |> List.filter_map (fun (i,opt) -> 
      match opt with
      | Some v -> Some (i,v)
      | None -> None)
end

(* Combining the above two with the help of a hash func - leading to a Table Map *)
module type TableMap = sig

  (** [('k,'v) t] is the type of mutable table-based maps
      that bind keys of type ['k] to values of type ['v]*)
  type ('k,'v) t

  (** [insert k v tab] mutates key [k] to bind with value [v].*)
  val insert : 'k -> 'v -> ('k, 'v) t -> unit

  (** [remove k tab] removes any binding with [k] in [tab].
      If [k] is not found in tm, tab is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> unit

  (** [find k tab] is [Some v] if tab binds [k] to [v] and [None] otherwise.*)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [create hash c] creates a new table map with capacity [c] that
      will use [hash] as the function to convert keys to integers.
      Requires: The output of [hash] is always non-negative, and [hash]
      runs in constant time. *)
  val create : ('k -> int) -> int -> ('k, 'v) t

  (** [bindings m] is an association list containing the same bindings
      as [m]. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list

  (** [of_list hash lst] creates a map with the same bindings as [lst],
      using [hash] as the hash function. Requires: [lst] does not
      contain any duplicate keys. *)
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t

end

module TableMap = struct
  (** AF:  If [buckets] is
        [| [(k11,v11); (k12,v12); ...];
           [(k21,v21); (k22,v22); ...];
           ... |]
      that represents the map
        {k11:v11, k12:v12, ...,
         k21:v21, k22:v22, ...,  ...}.
      RI: No key appears more than once in the array (so, no
        duplicate keys in association lists).  All keys are
        in the right buckets: if [k] is in [buckets] at index
        [b] then [hash(k) = b]. The output of [hash] must always
        be non-negative. [hash] must run in constant time. *)
  
   type ('k, 'v) t = {
      hash : 'k -> int;
      mutable size : int;
      mutable buckets : ('k * 'v) list array
   }

   let load_factor_lb = 0.5
   let load_factor_ub = 2.0

   let capacity tab = Array.length tab.buckets
   let get_idx k tab = tab.hash k mod capacity tab
   let get_lst k tab = tab.buckets.(get_idx k tab)
   let find k tab = tab.buckets.(get_idx k tab) |> List.assoc_opt k
   let mem k tab = 
      match find k tab with
      | None -> false
      | Some _ -> true

   let get_load_factor tab =
      float_of_int tab.size /. float_of_int (capacity tab)

   let create hash n =
      {hash = hash; size = 0; buckets = Array.make (max 1 n) []}
   
   (** [insert_no_resize k v tab] inserts a binding from [k] to [v] in [tab]
      and does not resize the table, regardless of what happens to the
      load factor.
      Efficiency: expected O(L). Driven by membership check of key to maintain RI*)
   let insert_no_resize k v tab = 
      let idx = get_idx k tab in
      let is_member = mem k tab in
      let old_lst = tab.buckets.(idx) in
      tab.buckets.(idx) <- (k,v) :: (old_lst |> List.filter (fun (key,_) -> key <> k));
      if not is_member then tab.size <- tab.size + 1 else ()
      
    (** [rehash tab new_capacity] replaces the buckets array of [tab] with a new
      array of size [new_capacity], and re-inserts all the bindings of [tab]
      into the new array.  The keys are re-hashed, so the bindings will
      likely land in different buckets.
      Efficiency: O(n), where n is the number of bindings. *)
   let rehash tab new_capacity =
      let old_buckets = tab.buckets in
      tab.buckets <- Array.make new_capacity [];
      let rec aux_pop = function
      | [] -> ()
      | (k, v) :: t -> 
         let idx = get_idx k tab in
         tab.buckets.(idx) <- (k,v) :: tab.buckets.(idx);
         aux_pop t
      in
      Array.iter aux_pop old_buckets

   
   (* [resize_if_needed tab] resizes and rehashes [tab] if the load factor
     is too big or too small.  Load factors are allowed to range from
     1/2 to 2. *)
   let resize_if_needed tab = 
      if get_load_factor tab > load_factor_ub
      then rehash tab (2 * (capacity tab))
      else ()

   let insert k v tab =
      insert_no_resize k v tab;
      resize_if_needed tab

   (** [remove k tab] removes [k] from [tab] and does not trigger
      a resize, regardless of what happens to the load factor.
      Efficiency: expected O(L). *)
   let remove k tab = 
      let idx = get_idx k tab in
      let old_bucket = tab.buckets.(idx) in
      tab.buckets.(idx) <- (tab.buckets.(idx) |> List.remove_assoc k);
      if (List.mem_assoc k old_bucket) 
      then tab.size <- tab.size - 1 
      else ()

   let bindings tab =
      let f acc (k,v) = (k,v) :: acc in
      Array.fold_left (List.fold_left f) [] tab.buckets
  
   let of_lst hash lst =
      let tab = create hash (List.length lst) in
      List.iter (fun (k,v) -> insert k v tab) lst;
      tab

end
