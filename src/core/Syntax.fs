namespace Core
open Basis
open Cubical
open Syntax

module SyntaxPrinter =

  //type s = SyntaxData< 's >

  let to_numeral =
    let rec go acc =
      function
      | Zero -> Some acc
      | Suc t -> go (acc+1) t
      | _ -> None
    in
    go 0

  let tm_abort = CofSplit []
  let tp_abort = TpCofSplit []

  let pp_path fmt p =
    fprintf fmt "%s" <|
    match p with
    | [] -> "."
    | _ -> String.concat "." p

  let rec dump fmt (v : SyntaxData< 'a >)=
    match v with
    | SyntaxData.Var(i) -> fprintf fmt "var[%i]" i
    | Global _ -> fprintf fmt "<global>"
    | Let _ -> fprintf fmt "<let>"
    | Ann _ -> fprintf fmt "<ann>"

    | Zero -> fprintf fmt "zero"
    | Suc tm -> fprintf fmt "suc[%a]" dump tm
    | NatElim _ -> fprintf fmt "<nat-elam>"

    | Base -> fprintf fmt "base"
    | Loop _ -> fprintf fmt "<loop>"
    | CircleElim _ -> fprintf fmt "<circle/elim>"

    | Lam (ident, tm) -> fprintf fmt "lam[%a, %a]" Ident.pp ident dump tm
    | Ap (tm0, tm1) -> fprintf fmt "ap[%a, %a]" dump tm0 dump tm1

    | Pair (tm0, tm1) -> fprintf fmt "pair[%a, %a]" dump tm0 dump tm1
    | Fst tm -> fprintf fmt "fst[%a]" dump tm
    | Snd tm -> fprintf fmt "snd[%a]" dump tm

    | Struct fields -> fprintf fmt "struct[%a]" dump_struct fields

    | Proj (tm, lbl) -> fprintf fmt "proj[%a, %a]" dump tm pp_path lbl
    | Coe _ -> fprintf fmt "<coe>"
    | HCom _ -> fprintf fmt "<hcom>"
    | Com _ -> fprintf fmt "<com>"

    | SubIn _ -> fprintf fmt "<sub/in>"
    | SubOut _ -> fprintf fmt "<sub/out>"

    | SyntaxData.Dim0 -> fprintf fmt "<dim0>"
    | SyntaxData.Dim1 -> fprintf fmt "<dim1>"
    | SyntaxData.Cof cof -> fprintf fmt "cof[%a]" dump_cof cof
    | ForallCof _ -> fprintf fmt "<dim1>"
    | CofSplit branches -> fprintf fmt "cof/split[%a]" (Pp.pp_sep_list dump_branch) branches
    | Prf -> fprintf fmt "prf"

    | ElIn tm -> fprintf fmt "el/in[%a]" dump tm
    | ElOut tm -> fprintf fmt "el/out[%a]" dump tm

    | Box _ -> fprintf fmt "<box>"
    | Cap _ -> fprintf fmt "<cap>"

    | VIn _ -> fprintf fmt "<vin>"
    | VProj _ -> fprintf fmt "<vproj>"

    | CodeExt _ -> fprintf fmt "<ext>"
    | CodePi _ -> fprintf fmt "<pi>"
    | CodeSg _ -> fprintf fmt "<sg>"
    | CodeSignature fields ->
      fprintf fmt "sig[%a]"
        (Pp.pp_sep_list (fun fmt (lbl, tp) -> fprintf fmt "%a : %a" pp_path lbl dump tp))
        fields
    | CodeNat -> fprintf fmt "nat"
    | CodeUniv -> fprintf fmt "univ"
    | CodeV _ -> fprintf fmt "<v>"
    | CodeCircle -> fprintf fmt "circle"

    | ESub _ -> fprintf fmt "<esub>"

    | LockedPrfIn _ -> fprintf fmt "<locked/in>"
    | LockedPrfUnlock _ -> fprintf fmt "<locked/unlock>"

  and dump_struct fmt fields =
    fprintf fmt "%a" (Pp.pp_sep_list (fun fmt (lbl, tp) -> fprintf fmt "%a : %a" pp_path lbl dump tp)) fields

  and dump_sign fmt sign =
    fprintf fmt "%a" (Pp.pp_sep_list (fun fmt (lbl, tp) -> fprintf fmt "%a : %a" pp_path lbl dump_tp tp)) sign

  and dump_tp fmt =
    function
    | Univ -> fprintf fmt "univ"
    | El t -> fprintf fmt "el[%a]" dump t
    | TpVar i -> fprintf fmt "tp/var[%i]" i
    | TpDim -> fprintf fmt "tp/dim"
    | TpCof -> fprintf fmt "tp/cof"
    | TpPrf t -> fprintf fmt "tp/prf[%a]" dump t
    | TpCofSplit _ -> fprintf fmt "<tp/cof/split>"
    | Sub _ -> fprintf fmt "<sub>"
    | Pi (_base, ident, fam) -> fprintf fmt "pi[%a, %a, %a]" dump_tp _base Ident.pp ident dump_tp fam
    | Sg _ -> fprintf fmt "<sg>"
    | Signature fields -> fprintf fmt "tp/sig[%a]" dump_sign fields
    | Nat -> fprintf fmt "nat"
    | Circle -> fprintf fmt "circle"
    | TpESub _ -> fprintf fmt "<esub>"
    | TpLockedPrf _ -> fprintf fmt "<locked>"


  and dump_cof fmt =
    function
    | Cof.Eq (r1, r2) -> fprintf fmt "eq[%a, %a]" dump r1 dump r2
    | Cof.Join cofs -> fprintf fmt "join[%a]" (Pp.pp_sep_list dump) cofs
    | Cof.Meet cofs -> fprintf fmt "meet[%a]" (Pp.pp_sep_list dump) cofs

  and dump_branch fmt (cof, bdy) =
    fprintf fmt "[%a, %a]" dump cof dump bdy

  module P =
    open SyntaxPrecedence
      (* anti-shadowing *)
    type t = SyntaxPrecedence

    let passed = nonassoc 12
    let atom = nonassoc 11
    let delimited = nonassoc 11
    let tuple = delimited
    let substitution = right 10
    let juxtaposition = left 9
    let proj = right 8
    let sub_compose = left 7
    let cof_eq = nonassoc 6
    let cof_meet = nonassoc 5
    let cof_join = nonassoc 5
    let sub_comma = left 4
    let arrow = right 3
    let times = right 3
    let colon = nonassoc 2
    let double_arrow = right 1
    let in_ = nonassoc 0

    (** assumes [Debug.is_debug_mode ()] = [false] *)
    let classify_tm : SyntaxData.SyntaxData<_> -> t =
      function
      | Var _ | Global _ -> atom
      | Lam _ -> double_arrow
      | Ap _ -> juxtaposition
      | Pair _ -> tuple
      | Struct _ -> juxtaposition
      | Proj _ -> proj
      | CofSplit _ -> tuple
      | Cof (Cof.Eq _) -> cof_eq
      | Cof (Cof.Join [] | Cof.Meet []) -> atom
      | Cof (Cof.Join _) -> cof_join
      | Cof (Cof.Meet _) -> cof_meet
      | ForallCof _ -> dual juxtaposition arrow

      | Zero | Base | CodeNat | CodeCircle | CodeUniv | Dim0 | Dim1 | Prf -> atom
      | Suc _ as tm -> if Option.is_some (to_numeral tm) then atom else juxtaposition
      | HCom _ | Com _ | Coe _
      | Fst _ | Snd _
      | NatElim _ | Loop _
      | CircleElim _ -> juxtaposition

      | SubIn _ | SubOut _ | ElIn _ | ElOut _ -> passed
      | CodePi _ -> arrow
      | CodeSg _ -> times
      | CodeSignature _ -> juxtaposition
      | CodeExt _ -> juxtaposition

      | Ann _ -> passed
      | Let _ -> dual juxtaposition in_

      | Box _ -> tuple
      | Cap _ -> juxtaposition
      | CodeV _ -> juxtaposition
      | VIn _ -> tuple
      | VProj _ -> juxtaposition
      | ESub _ -> juxtaposition
      | LockedPrfIn _ -> juxtaposition
      | LockedPrfUnlock _ -> delimited

    let classify_sub : SyntaxData.sub<_> -> t =
      function
      | SbI | Sb1 | SbP -> atom
      | SbC _ -> sub_compose
      | SbE _ -> sub_comma

    let classify_tp : SyntaxData.tp<_> -> t =
      function
      | Univ | TpDim | TpCof | Nat | Circle -> atom
      | El _ -> passed
      | TpVar _ -> atom
      | TpPrf _ -> delimited
      | TpCofSplit _ -> delimited
      | Sub _ -> juxtaposition
      | Pi _ -> arrow
      | Sg _ -> times
      | Signature _ -> juxtaposition
      | TpESub _ -> substitution
      | TpLockedPrf _ -> juxtaposition
  

  let pp_var env fmt ix =
    Uuseg_string.pp_utf_8 fmt @@ Pp.Env.var ix env

  let pp_bracketed pp fmt a =
    fprintf fmt "@[<hv>[ %a@ ]@]"
      pp a

  let pp_bracketed_list ~pp_sep pp fmt xs =
    pp_bracketed (pp_print_list ~pp_sep pp) fmt xs

  let pp_tuple pp =
    let pp_sep fmt () = fprintf fmt "@ , " in
    pp_bracketed_list ~pp_sep pp

  let pp_braced pp fmt a =
    fprintf fmt "{%a}"
      pp a

  let pp_braced_cond classify plain_pp penv fmt tm =
    if P.parens penv @@ classify tm then
      pp_braced (plain_pp penv) fmt tm
    else
      plain_pp penv fmt tm

  let ppenv_bind env ident =
    Pp.Env.bind env @@ Ident.to_string_opt ident

  let rec pp_fields pp_field env fmt  =
    function
    | [] -> ()
    | ((lbl, tp) :: fields) ->
      fprintf fmt "(%a : %a)@ @,%a"
        pp_path lbl
        (pp_field env P.(right_of colon)) tp
        (pp_fields pp_field env) fields

  let rec pp env =
    pp_braced_cond P.classify_tm @@ fun penv fmt ->
    function
    | Lam _ as tm ->
      fprintf fmt "@[%a@]"
        (pp_lambdas env) tm
    | Ap (tm0, tm1) ->
      fprintf fmt "%a %a"
        (pp env P.(left_of juxtaposition)) tm0 (pp_atomic env) tm1
    | Pair (tm0, tm1) ->
      pp_tuple (pp env P.isolated) fmt [tm0; tm1]
    | Struct fields ->
      fprintf fmt "@[struct %a@]" (pp_fields pp env) fields
    | Proj (tm, lbl) ->
      fprintf fmt "@[%a %@ %a@]" (pp env P.(left_of proj)) tm pp_path lbl
    | CofSplit branches ->
      let pp_sep fmt () = fprintf fmt "@ | " in
      pp_bracketed_list ~pp_sep (pp_cof_split_branch env) fmt branches
    | HCom (code, r, s, phi, bdy) ->
      fprintf fmt "@[<hv2>hcom %a %a %a %a@ %a@]"
        (pp_atomic env) code
        (pp_atomic env) r
        (pp_atomic env) s
        (pp_atomic env) phi
        (pp_atomic env) bdy
    | Com (fam, r, s, phi, bdy) ->
      fprintf fmt "@[<hv2>com %a %a %a %a@ %a@]"
        (pp_atomic env) fam
        (pp_atomic env) r
        (pp_atomic env) s
        (pp_atomic env) phi
        (pp_atomic env) bdy
    | Coe (fam, r, s, bdy) ->
      fprintf fmt "@[<hv2>coe %a %a %a@ %a@]"
        (pp_atomic env) fam
        (pp_atomic env) r
        (pp_atomic env) s
        (pp_atomic env) bdy
    | Var ix ->
      pp_var env fmt ix
    | Global sym ->
      Symbol.pp fmt sym
    | Cof (Cof.Eq (r, s)) ->
      fprintf fmt "%a = %a" (pp env P.(left_of cof_eq)) r (pp env P.(right_of cof_eq)) s
    | Cof (Cof.Join []) ->
      fprintf fmt "#f"
    | Cof (Cof.Join phis) ->
      let pp_sep fmt () = Uuseg_string.pp_utf_8 fmt " ∨ " in
      pp_print_list ~pp_sep (pp env P.(surrounded_by cof_join)) fmt phis
    | Cof (Cof.Meet []) ->
      fprintf fmt "#t"
    | Cof (Cof.Meet phis) ->
      let pp_sep fmt () = Uuseg_string.pp_utf_8 fmt " ∧ " in
      pp_print_list ~pp_sep (pp env P.(surrounded_by cof_meet)) fmt phis
    | ForallCof phi ->
      let x, envx = ppenv_bind env `Anon in
      fprintf fmt "%a %a %a %a"
        Uuseg_string.pp_utf_8 "∀"
        Uuseg_string.pp_utf_8 x
        Uuseg_string.pp_utf_8 "→"
        (pp envx P.(right_of arrow)) phi
    | Fst tm ->
      fprintf fmt "fst %a" (pp_atomic env) tm
    | Snd tm ->
      fprintf fmt "snd %a" (pp_atomic env) tm
    | Zero ->
      fprintf fmt "0"
    | Suc tm ->
      begin
        match to_numeral tm with
        | Some n -> fprintf fmt "%i" (n + 1)
        | None -> fprintf fmt "suc %a" (pp_atomic env) tm
      end
    | NatElim (mot, zero, suc, tm) ->
      fprintf fmt "@[<hv2>elim %a %@ %a@ @[<v>[ zero => %a@ | suc => %a@ ]@]@]"
        (pp_atomic env) tm
        (pp_atomic env) mot
        (pp env P.isolated) zero
        (pp env P.isolated) suc
    | Base ->
      fprintf fmt "base"
    | Loop tm ->
      fprintf fmt "loop %a" (pp_atomic env) tm
    | CircleElim (mot, base, loop, tm) ->
      fprintf fmt "@[<hv2>elim %a %@ %a@ @[<v>[ base => %a@ | loop => %a@ ]@]@]"
        (pp_atomic env) tm
        (pp_atomic env) mot
        (pp env P.isolated) base
        (pp env P.isolated) loop
    | SubIn tm when Debug.is_debug_mode () ->
      fprintf fmt "sub/in %a" (pp_atomic env) tm
    | SubOut tm when Debug.is_debug_mode () ->
      fprintf fmt "sub/out %a" (pp_atomic env) tm
    | ElIn tm when Debug.is_debug_mode () ->
      fprintf fmt "el/in %a" (pp_atomic env) tm
    | ElOut tm when Debug.is_debug_mode () ->
      fprintf fmt "el/out %a" (pp_atomic env) tm
    | SubIn tm | SubOut tm | ElIn tm | ElOut tm ->
      pp env penv fmt tm

    | CodePi (base, fam) when Debug.is_debug_mode () ->
      fprintf fmt "@[%a %a %a@]"
        Uuseg_string.pp_utf_8 "<∏>"
        (pp_atomic env) base
        (pp_atomic env) fam
    | CodePi (base, Lam (ident, fam)) ->
      let x, envx = ppenv_bind env ident in
      fprintf fmt "(%a : %a) %a %a"
        Uuseg_string.pp_utf_8 x
        (pp env P.(right_of colon)) base
        Uuseg_string.pp_utf_8 "→"
        (pp envx P.(right_of arrow)) fam
    | CodePi (base, tm) ->
      fprintf fmt "@[%a %a %a@]"
        Uuseg_string.pp_utf_8 "∏"
        (pp_atomic env) base
        (pp_atomic env) tm

    | CodeSg (base, fam) when Debug.is_debug_mode () ->
      fprintf fmt "@[%a %a %a@]"
        Uuseg_string.pp_utf_8 "<Σ>"
        (pp_atomic env) base
        (pp_atomic env) fam
    | CodeSg (base, Lam (ident, fam)) ->
      let x, envx = ppenv_bind env ident in
      fprintf fmt "(%a : %a) %a %a"
        Uuseg_string.pp_utf_8 x
        (pp env P.(right_of colon)) base
        Uuseg_string.pp_utf_8 "×"
        (pp envx P.(right_of times)) fam
    | CodeSg (base, tm) ->
      fprintf fmt "@[%a %a %a@]"
        Uuseg_string.pp_utf_8 "Σ"
        (pp_atomic env) base
        (pp_atomic env) tm
    | CodeSignature fields ->
      fprintf fmt "@[sig %a@]" (pp_fields pp_binders env) fields
    | CodeExt (_, fam, `Global phi, bdry) ->
      fprintf fmt "@[ext %a %a %a@]"
        (pp_atomic env) fam
        (pp_atomic Pp.Env.emp) phi
        (pp_atomic env) bdry

    | CodeNat when Debug.is_debug_mode () ->
      fprintf fmt "`nat"
    | CodeCircle when Debug.is_debug_mode () ->
      fprintf fmt "`circle"
    | CodeUniv when Debug.is_debug_mode () ->
      fprintf fmt "`type"
    | CodeNat ->
      fprintf fmt "nat"
    | CodeCircle ->
      fprintf fmt "circle"
    | CodeUniv ->
      fprintf fmt "type"

    | Dim0 ->
      fprintf fmt "0"
    | Dim1 ->
      fprintf fmt "1"
    | Prf ->
      fprintf fmt "*"
    | Ann (tm, _) ->
      pp env penv fmt tm
    | Let (tm, ident, bdy) ->
      let x, envx = ppenv_bind env ident in
      fprintf fmt "@[let %a = %a in@ %a@]"
        Uuseg_string.pp_utf_8 x
        (pp env P.isolated) tm
        (pp envx P.(right_of in_)) bdy
    | Box (r, s, phi, sides, cap) when Debug.is_debug_mode () ->
      fprintf fmt "@[<hv2>box %a %a %a %a %a@]"
        (pp_atomic env) r
        (pp_atomic env) s
        (pp_atomic env) phi
        (pp_atomic env) sides
        (pp_atomic env) cap
    | Box (_r, _s, _phi, sides, cap) ->
      pp_tuple (pp env P.isolated) fmt [sides; cap]
    | Cap (r, s, phi, code, box) when Debug.is_debug_mode ()->
      fprintf fmt "@[<hv2>cap %a %a %a %a %a@]"
        (pp_atomic env) r
        (pp_atomic env) s
        (pp_atomic env) phi
        (pp_atomic env) code
        (pp_atomic env) box
    | Cap (_r, _s, _phi, _code, box) ->
      fprintf fmt "@[<hv2>cap %a@]" (pp_atomic env) box
    | CodeV (r, pcode, code, pequiv) ->
      fprintf fmt "@[<hv2>V %a %a %a %a@]"
        (pp_atomic env) r
        (pp_atomic env) pcode
        (pp_atomic env) code
        (pp_atomic env) pequiv
    | VIn (r, equiv, pivot, base) when Debug.is_debug_mode () ->
      fprintf fmt "@[<hv2>vin %a %a %a %a@]"
        (pp_atomic env) r
        (pp_atomic env) equiv
        (pp_atomic env) pivot
        (pp_atomic env) base
    | VIn (_, _, pivot, base) ->
      pp_tuple (pp env P.isolated) fmt [pivot; base]
    | VProj (r, pcode, code, pequiv, v) when Debug.is_debug_mode () ->
      fprintf fmt "@[<hv2>vproj %a %a %a %a %a@]"
        (pp_atomic env) r
        (pp_atomic env) pcode
        (pp_atomic env) code
        (pp_atomic env) pequiv
        (pp_atomic env) v
    | VProj (_, _, _, _, v) ->
      fprintf fmt "@[<hv2>vproj %a@]"
        (pp_atomic env) v

    | ESub (sub, tm) ->
      fprintf fmt "[%a]%a"
        (pp_sub env P.isolated) sub
        (pp env P.(right_of substitution)) tm

    | LockedPrfIn prf ->
      fprintf fmt "@[<hv2>lock %a@]"
        (pp_atomic env) prf

    | LockedPrfUnlock {cof; prf; bdy; _} ->
      fprintf fmt "@[unlock %a : %a in@ %a@]"
        (pp env P.(left_of colon)) prf
        (pp env P.(right_of colon)) cof
        (pp env P.(right_of in_)) bdy

  and pp_sub env =
    pp_braced_cond P.classify_sub @@ fun _ fmt ->
    function
    | Sb1 ->
      Uuseg_string.pp_utf_8 fmt "ε"
    | SbP ->
      fprintf fmt "p"
    | SbI ->
      fprintf fmt "id"
    | SbE (sb, tm) ->
      fprintf fmt "%a, %a"
        (pp_sub env P.(left_of sub_comma)) sb
        (pp env P.(right_of sub_comma)) tm
    | SbC (sb0, sb1) ->
      fprintf fmt "%a %a %a"
        (pp_sub env P.(left_of sub_compose)) sb0
        Uuseg_string.pp_utf_8 "∘"
        (pp_sub env P.(right_of sub_compose)) sb1

  and pp_sign env fmt (sign : sign) : unit = pp_fields pp_tp env fmt sign

  and pp_tp env =
    pp_braced_cond P.classify_tp @@ fun penv fmt ->
    function
    | TpCofSplit branches ->
      let pp_sep fmt () = fprintf fmt "@ | " in
      pp_bracketed_list ~pp_sep
        (pp_tp_cof_split_branch env)
        fmt
        branches
    | Pi (base, ident, fam) ->
      let x, envx = ppenv_bind env ident in
      fprintf fmt "(%a : %a) %a %a"
        Uuseg_string.pp_utf_8 x
        (pp_tp env P.(right_of colon)) base
        Uuseg_string.pp_utf_8 "→"
        (pp_tp envx P.(right_of arrow)) fam
    | Sg (base, ident, fam) ->
      let x, envx = ppenv_bind env ident in
      fprintf fmt "(%a : %a) %a %a"
        Uuseg_string.pp_utf_8 x
        (pp_tp env P.(right_of colon)) base
        Uuseg_string.pp_utf_8 "×"
        (pp_tp envx P.(right_of times)) fam
    | Signature fields ->
      fprintf fmt "sig %a" (pp_sign env) fields
    | Sub (tp, phi, tm) ->
      let _x, envx = ppenv_bind env `Anon in
      fprintf fmt "@[sub %a %a@ %a@]"
        (pp_tp env P.(right_of juxtaposition)) tp
        (pp_atomic env) phi
        (pp_atomic envx) tm
    | TpDim ->
      fprintf fmt "𝕀"
    | TpCof ->
      fprintf fmt "𝔽"
    | Univ ->
      fprintf fmt "type"
    | Nat ->
      fprintf fmt "nat"
    | Circle ->
      fprintf fmt "circle"
    | El tm when Debug.is_debug_mode () ->
      fprintf fmt "el %a" (pp_atomic env) tm
    | El tm ->
      pp env penv fmt tm
    | TpVar ix ->
      fprintf fmt "#var[%i]" ix
    | TpPrf cof ->
      pp_bracketed (pp env P.isolated) fmt cof
    | TpESub (sub, tp) ->
      fprintf fmt "[%a]%a"
        (pp_sub env P.isolated) sub
        (pp_tp env P.(right_of substitution)) tp
    | TpLockedPrf phi ->
      fprintf fmt "locked %a"
        (pp_atomic env) phi

  and pp_cof_split_branch env fmt (phi, tm) =
    let _x, envx = ppenv_bind env `Anon in
    fprintf fmt "@[<hv>%a =>@ %a@]"
      (pp env P.(left_of double_arrow)) phi
      (pp envx P.(right_of double_arrow)) tm

  and pp_tp_cof_split_branch env fmt (phi, tm) =
    let _x, envx = ppenv_bind env `Anon in
    fprintf fmt "@[<hv>%a =>@ %a@]"
      (pp env P.(left_of double_arrow)) phi
      (pp_tp envx P.(right_of double_arrow)) tm

  (* XXX [pp_atomic] should have been removed, but it was kept to minimize git diff. It now means printing the term to the right of the juxtaposition operator, like [arg] in [f arg]. The fine-grained control brought by {!module:SyntaxPrecedence} obsoletes the old classification of terms. *)
  and pp_atomic env fmt tm =
    pp env P.(right_of juxtaposition) fmt tm

  and pp_lambdas env fmt tm =
    match tm with
    | Lam (nm, tm) ->
      let x, envx = ppenv_bind env nm in
      fprintf fmt "%a %a"
        Uuseg_string.pp_utf_8 x
        (pp_lambdas envx) tm
    | (SubIn tm | SubOut tm | ElIn tm | ElOut tm) when not @@ Debug.is_debug_mode () ->
      pp_lambdas env fmt tm
    | _ ->
      fprintf fmt "=>@ @[%a@]"
        (pp env P.(right_of double_arrow)) tm

  (* Pretty print a term that uses lambdas as binders. *)
  and pp_binders env penv fmt tm =
    match tm with
    | Lam (nm, tm) ->
      let _, envx = ppenv_bind env nm in
      pp_binders envx penv fmt tm
    | _ -> pp env penv fmt tm

  let pp_sequent_boundary env fmt tm =
    let rec pp_branches env fmt (bdry, cons) =
      match cons with
      | CofSplit branches ->
        let _x, envx = ppenv_bind env `Anon in
        pp_print_list ~pp_sep:(pp_print_cut) (pp_branches envx) fmt branches
      | _ -> pp_cof_split_branch env fmt (bdry, cons)
    in
    match tm with
    | CofSplit branches when not (CCList.is_empty branches) ->
      pp_print_list ~pp_sep:(pp_print_cut) (pp_branches env) fmt branches
    | _ -> pp env P.isolated fmt tm

  let rec pp_in_ctx env ctx pp_goal fmt goal =
    match ctx with
    | [] -> pp_goal env fmt goal
    | (var, var_tp) :: ctx ->
      let x, envx = ppenv_bind env var in
      Fmt.fprintf fmt "%a : %a@;%a"
        Uuseg_string.pp_utf_8 x
        (pp_tp env P.(right_of colon)) var_tp
        (pp_in_ctx envx ctx pp_goal) goal

  let rec get_constraints =
    function
    | Sub (tp, Cof (Cof.Join []), _) -> get_constraints tp
    | Sub (tp, phi, tm) -> `Boundary (tp, phi, tm)
    | El (CodeExt (0, tp, `Global phi, (Lam (_, tm)))) -> `Boundary (El tp, phi, tm)
    | tp -> `Unconstrained tp

  let pp_sequent_goal ~lbl env fmt tp  =
    let lbl = Option.value ~default:"" lbl in
    match get_constraints tp with
    | `Boundary (tp, phi, tm) ->
      let _x, envx = Pp.Env.bind env (Some "_") in
      fprintf fmt "|- ?%a : @[<hov>%a@]@,@,Boundary:@,%a@,|- @[<v>%a@]"
        Uuseg_string.pp_utf_8 lbl
        (pp_tp env P.(right_of colon)) tp
        (pp env P.(right_of colon))
        phi
        (pp_sequent_boundary envx)
        tm
    | `Unconstrained tp ->
      fprintf fmt "|- ?%a : @[<hov>%a@]"
        Uuseg_string.pp_utf_8 lbl
        (pp_tp env P.(right_of colon)) tp

  let pp_sequent ~lbl ctx : tp Pp.printer =
    fun fmt tp ->
    fprintf fmt "@[<v>%a@]"
      (pp_in_ctx Pp.Env.emp ctx (pp_sequent_goal ~lbl)) tp

  let pp_boundary_sat fmt =
    function
    | `BdrySat -> pp_print_string fmt "satisfied"
    | `BdryUnsat -> pp_print_string fmt "unsatisfied"

  let pp_partial_sequent_goal bdry_sat env fmt (partial, tp) =
    match get_constraints tp with
    | `Boundary (tp, phi, tm) ->
      let _x, envx = Pp.Env.bind env (Some "_") in
      fprintf fmt "|- {! %a !} : @[<hov>%a@]@,@,Boundary (%a):@,%a@,|- @[<v>%a@]"
        (pp env P.(right_of colon)) partial
        (pp_tp env P.(right_of colon)) tp
        pp_boundary_sat bdry_sat
        (pp env P.(right_of colon)) phi
        (pp_sequent_boundary envx) tm
    | `Unconstrained tp ->
      fprintf fmt "|- {! %a !} : @[<hov>%a@]"
        (pp env P.(right_of colon)) partial
        (pp_tp env P.(right_of colon)) tp

  let pp_partial_sequent bdry_sat ctx : (t * tp) Pp.printer =
    fun fmt goal ->
    fprintf fmt "@[<v>%a@]"
      (pp_in_ctx Pp.Env.emp ctx (pp_partial_sequent_goal bdry_sat)) goal

  let pp env = pp env P.isolated
  let pp_tp env = pp_tp env P.isolated
end
