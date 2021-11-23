namespace Basis

//type 'a printer = formatter -> 'a -> unit

open Bwd
open BwdNotation

module Env =

  let emp = Emp

  let nat_to_suffix n =
    let formatted = sprintf "%i" n 
    let lookup : int -> string = List.nth ["₀";"₁";"₂";"₃";"₄";"₅";"₆";"₇";"₈";"₉"]
    String.concat "" 
    <| List.init (String.length formatted) (fun n -> lookup (Char.code (String.get formatted n) - Char.code '0'))

  let rec rename xs x i =
    let suffix = nat_to_suffix i 
    let new_x = x + suffix 
    if Bwd.mem new_x xs then rename  xs x (i + 1) else new_x

  let choose_name (env : string bwd) (x : string) =
    if Bwd.mem x env then rename env x 1 else x

  let var i env =
    if i < Bwd.length env then
      Bwd.nth env i
    else
      failwith "Pp printer: tried to resolve bound variable out of range"

  let proj xs =
    match xs with
    | Emp -> failwith "ppenv/proj"
    | Snoc (xs, _) -> xs

  let bind (env : string bwd) (nm : string option) : string * string bwd =
    let x =
      match nm with
      | None -> choose_name env "_x"
      | Some x -> choose_name env x
    in
    x, env %< x

  let rec bindn (env : string bwd) (nms : string option list) : string list * string bwd =
    match nms with
    | [] ->
      [], env
    | nm :: nms ->
      let x, env' = bind env nm in
      let xs, env'' = bindn env' nms in
      (x :: xs), env''

  let names (env : string bwd) : string list =
    env <>> []


let pp_sep_list sep pp_elem fmt xs =
  pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt sep) pp_elem fmt xs
