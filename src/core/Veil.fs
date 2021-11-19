module Core.Veil

open Basis 

//module Global = CodeUnit.Global
//module SymbolMap = SymbolMap.Make (Global)

type policy =  | Translucent | Transparent


type Policy = {Default : policy; Custom : policy SymbolMap.t}

let policy : CodeUnit.Global -> Policy -> policy =
  fun sym veil ->
  match SymbolMap.find_opt sym veil.custom with
  | Some p -> p
  | None -> veil.Default

let unfold syms veil =
  {veil with
   Custom =
     List.foldBack (fun m sym -> SymbolMap.add sym Transparent m) syms veil.custom}

let Const : policy -> Policy =
  fun p ->
  {Default = p; Custom = SymbolMap.empty}
