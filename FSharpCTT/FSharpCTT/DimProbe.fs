namespace Cubical

open System

type DimProbe = int

//module J = Ezjsonm
//
//type t = int
//
module DimProbe =
  let ``global`` = ref 0

  let compare (p1 : DimProbe) (p2 : DimProbe) = p1.CompareTo(p2)

  let equal (p1 : DimProbe) (p2 : DimProbe) = p1 = p2

  let pp fmt p =
    fprintf fmt "#%i" p

  let show p = p.ToString()

//  let serialize (p : t) : J.value =
//    `String (Int.to_string p)
//
//  let deserialize : J.value -> t =
//    function
//    | `String p -> int_of_string p
//    | j -> J.parse_error j "DimProbe.deserialize"



  let fresh () =
    let i = !``global`` in
    ``global`` := i + 1;
    i
