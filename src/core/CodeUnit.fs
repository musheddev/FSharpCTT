module Core.CodeUnit

//open ContainersLabels
open Basis




type CodeUnitId  = string option
let compare = Option.compare String.compare
let pp fmt id = Format.pp_print_string fmt (Option.value ~default:"<top-level>" id)
let top_level : CodeUnitId = None
let file s : CodeUnitId = Some s

type id = CodeUnitID


type Global =
  { origin : CodeUnitId;
    index : int;
    name : string option }
  ///[@@deriving show]

let compare s1 s2 =
  Int.compare s1.index s2.index

let equal s1 s2 =
  s1.index = s2.index

//let pp fmt sym =
//  match sym.name with
//  | Some nm ->
//    Format.sprintf fmt "%a" Uuseg_string.pp_utf_8 nm
//  | None ->
//    Format.fprintf fmt "#%i" sym.index
//
//let serialize sym =
//  `O [("origin", J.option J.string @@ sym.origin);
//      ("index", `String (Int.to_string sym.index));
//      ("name", J.option J.string @@ sym.name) ]
//
//let deserialize : J.value -> t =
//  function
//  | `O [("origin", j_origin); ("index", j_index); ("name", j_name)] ->
//    { origin = J.get_option J.get_string j_origin;
//      index = int_of_string @@ J.get_string j_index;
//      name = J.get_option J.get_string j_name }
//  | j -> J.parse_error j "Global.deserialize"


module Domain = Domain.Make (Global)
module Syntax = Syntax.Make (Global)


type CodeUnit =
  { (* The name of the code unit.  *)
    id : id;
    (* All the top-level bindings for this code unit. *)
    symbol_table :  (Domain.tp * Domain.con option) Vector.vector }

let origin (sym : Global) = sym.origin

let id code_unit = code_unit.id

let create id =
  { id = id;
    symbol_table = Vector.create () }

let add_global ident tp ocon code_unit =
  let index = Vector.length code_unit.symbol_table in
  let _ = Vector.push code_unit.symbol_table (tp, ocon) in
  let sym = { Global.origin = code_unit.id; index = index; name = Ident.to_string_opt ident } in
  (sym, code_unit)

let get_global (sym : Global.t) code_unit =
  Vector.get code_unit.symbol_table sym.index

