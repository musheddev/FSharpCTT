namespace Core

//open ContainersLabels
open Basis



type CodeUnitId  = | Id of string | TopLevel
//let compare = Option.compare String.compare
  with 
  static member pp fmt id = fprintf fmt "%s" (Option.defaultValue "<top-level>" id)
  static member top_level : CodeUnitId = TopLevel
  static member file s : CodeUnitId = Id(s)

//type id = CodeUnitID


type Global =
  { origin : CodeUnitId;
    index : int;
    name : string option }
  ///[@@deriving show]
  
with
  static member compare (s1 : Global) (s2 : Global) = s1.index.CompareTo(s2.index)

  static member equal s1 s2 =
    s1.index = s2.index
