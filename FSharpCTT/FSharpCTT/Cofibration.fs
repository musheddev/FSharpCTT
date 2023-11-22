namespace Cubical
type ('r, 'a) Cof_f =
  | Eq of 'r * 'r
  | Join of 'a list
  | Meet of 'a list


type ('r, 'v) Cof =
  | Cof of ('r, ('r, 'v) Cof) Cof_f
  | Var of 'v

module Cof =
  let var v = Cof.Var v
  let bot = Cof (Join [])
  let top = Cof (Meet [])

  let eq x y =
    if x = y then top else Cof (Eq (x, y))

  let join2 phi psi =
    match phi, psi with
    | Cof (Meet []), _ | _, Cof (Meet []) -> Cof (Meet [])
    | phi, psi ->
      let expose = function Cof (Join phis) -> phis | phi -> [phi] in
      match expose phi @ expose psi with [phi] -> phi | l -> Cof (Join l)

  let meet2 phi psi =
    match phi, psi with
    | Cof (Join []), _ | _, Cof (Join []) -> Cof (Join [])
    | phi, psi ->
      let expose = function Cof (Meet phis) -> phis | phi -> [phi] in
      match expose phi @ expose psi with [phi] -> phi | l -> Cof (Meet l)

  let join l = List.foldBack join2 l bot 
  let meet l = List.foldBack meet2 l top 

  let boundary dim0 dim1 r = join [eq r dim0; eq r dim1]

  let complexity_cof_f complexity_a =
    function
    | Eq _ -> 1
    | Join l | Meet l -> List.foldBack (fun c i -> i + complexity_a c) l 1

  let rec complexity_cof =
    function
    | Cof cof -> 1 + complexity_cof_f complexity_cof cof
    | Var _ -> 1

  let dump_cof_f dump_r dump_a fmt =
    function
    | Eq (r1, r2) -> fprintf fmt "eq[%a;%a]" dump_r r1 dump_r r2
    | Join l ->
      fprintf fmt "join[%A]" l
        //(pp_print_list pp_sep:(fun fmt () -> pp_print_char fmt ';') dump_a) lS
    | Meet l ->
      fprintf fmt "meet[%A]" l
        //(pp_print_list pp_sep:(fun fmt () -> pp_print_char fmt ';') dump_a) l

  let rec dump_cof dump_r dump_v fmt =
    function
    | Cof cof -> dump_cof_f dump_r (dump_cof dump_r dump_v) fmt cof
    | Var v -> dump_v fmt v
