module Core.Veil

open Basis 

//module Global = CodeUnit.Global


type PolicyType =  | Translucent | Transparent
type SymbolMap = Map<Global,PolicyType>

type Policy = {Default : PolicyType; Custom : SymbolMap}

let _SymbolMap : SymbolMap = Map.empty

let policy : Global -> Policy -> PolicyType =
  fun sym veil ->
  match _SymbolMap.TryFind(sym) with //veil.custom with
  | Some p -> p
  | None -> veil.Default

let unfold (syms : list<Global>, veil) =
  {veil with
    Custom =
      List.foldBack (fun sym m -> Map.add sym Transparent m) syms veil.Custom}

let Const : PolicyType -> Policy =
  fun p ->
  {Default = p; Custom = Map.empty}
