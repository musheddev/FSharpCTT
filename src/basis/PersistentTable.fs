namespace Basis
// module type S =
// sig
//   type key
//   type 'a t

//   val empty : 'a t
//   val size : 'a t -> int
//   val get : key -> 'a t -> 'a
//   val get_opt : key -> 'a t -> 'a option
//   val set : key -> 'a -> 'a t -> 'a t
//   val mem : key -> 'a t -> bool
//   val remove : key -> 'a t -> 'a t
//   val set_opt : key -> 'a option -> 'a t -> 'a t
//   val find : key -> 'a t -> 'a option
//   val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

//   (** entries from the first argument overwrite the ones from the second. *)
//   val merge : 'a t -> 'a t -> 'a t

//   val to_list : 'a t -> (key * 'a) list
//   val to_list_keys : 'a t -> key list
//   val to_list_values : 'a t -> 'a list
// end

module PersistentTable =

  //type key = O.t

  //module M = Map.Make (O)

  //type 'a t = 'a M.t

  let empty = Map.empty

  let size t = Map.count t

  let get k t = Map.find k t

  let get_opt k t = Map.tryFind k t

  let mem k t = Map.containsKey k t

  let find k t = Map.tryFind k t

  let set k v t = Map.add k v t

  let remove k t = Map.remove k t

  let set_opt k ov t = Map.change k (fun _ -> ov) t

  let fold f t e = Map.fold f e t 

  let merge (t0 : Map<'a,'b>) (t1 : Map<'a,'b>) = Map.fold (Map.add) t0 t1

  let to_list t = Map.toList t

  let to_list_keys t = List.map (fun (k, _) -> k) <| to_list t

  let to_list_values t = List.map (fun (_, v) -> v) <| to_list t


