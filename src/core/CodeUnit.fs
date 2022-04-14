namespace Core

//open ContainersLabels
open Basis


type CodeUnit =
  { (* The name of the code unit.  *)
    id : CodeUnitId;
    (* All the top-level bindings for this code unit. *)
    symbol_table :  (DomainData.tp<Global> * DomainData.con<Global> option) [] }

module CodeUnit =
  let origin (sym : Global) = sym.origin

  let id code_unit = code_unit.id

  let create id =
    { id = id;
      symbol_table = [||] }

  let add_global ident tp ocon code_unit =
    let index = code_unit.symbol_table.Length in
    let _ = Array.append code_unit.symbol_table [|(tp, ocon)|] in
    let sym = { Global.origin = code_unit.id; Global.index = index; Global.name = Ident.to_string_opt ident } in
    (sym, code_unit)

  let get_global (sym : Global) code_unit =
    code_unit.symbol_table.[sym.index]

