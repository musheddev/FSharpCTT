module Core.Domaain


open Basis
open Cubical



let const_tp_clo tp =
  Clo (S.TpVar 0, {tpenv = Snoc (Emp, tp); conenv = Emp})

let const_tm_clo con =
  Clo (S.Var 0, {tpenv = Emp; conenv = Snoc (Emp, con)})

let push frm (hd, sp) =
  hd, sp @ [frm]

let mk_var tp lvl =
  Cut {tp; cut = Var lvl, []}

let un_lam con =
  (* y, x |= y(x) *)
  Clo (S.Ap (S.Var 1, S.Var 0), {tpenv = Emp; conenv = Snoc (Emp, con)})

let compose f g =
  Lam (`Anon, Clo (S.Ap (S.Var 2, S.Ap (S.Var 1, S.Var 0)), {tpenv = Emp; conenv = Snoc (Snoc (Emp, f), g)}))

let apply_to x =
  Clo (S.Ap (S.Var 0, S.Var 1), {tpenv = Emp; conenv = Snoc (Emp, x)})

let fst = Lam (`Anon, Clo (S.Fst (S.Var 0), {tpenv = Emp; conenv = Emp}))
let snd = Lam (`Anon, Clo (S.Snd (S.Var 0), {tpenv = Emp; conenv = Emp}))

let proj lbl = Lam (`Anon, Clo (S.Proj (S.Var 0, lbl), {tpenv = Emp; conenv = Emp}))
let el_out = Lam (`Anon, Clo (S.ElOut (S.Var 0), {tpenv = Emp; conenv = Emp}))

let tm_abort = Split []
let tp_abort = TpSplit []

let sign_lbls =
  function
  | Field (lbl, _, Clo (sign, _)) -> lbl :: (List.map (fun (lbl, _) -> lbl) sign)
  | Empty -> []

let dim_to_con =
  function
  | Dim.Dim0 -> Dim0
  | Dim.Dim1 -> Dim1
  | Dim.DimVar lvl ->
    Cut {tp = TpDim; cut = Var lvl, []}
  | Dim.DimProbe sym ->
    DimProbe sym

let rec cof_to_con =
  function
  | Cof.Cof (Cof.Eq (r, s)) -> Cof (Cof.Eq (dim_to_con r, dim_to_con s))
  | Cof.Cof (Cof.Join phis) -> Cof (Cof.Join (List.map cof_to_con phis))
  | Cof.Cof (Cof.Meet phis) -> Cof (Cof.Meet (List.map cof_to_con phis))
  | Cof.Var lvl -> Cut {tp = TpCof; cut = Var lvl, []}

let pp_lsq fmt () = fprintf fmt "["
let pp_rsq fmt () = fprintf fmt "]"

let pp_list_group ~left ~right ~sep pp fmt xs =
  fprintf fmt "@[<hv0>%a %a@ %a@]"
    left ()
    (pp_print_list ~pp_sep:sep pp) xs
    right ()

let pp_path fmt p =
  Uuseg_string.pp_utf_8 fmt @@
  match p with
  | [] -> "."
  | _ -> String.concat "." p

let rec pp_cut : cut Pp.printer =
  fun fmt ->
  function
  | hd, sp ->
    fprintf fmt "%a <: %a"
      pp_hd hd
      pp_spine sp

and pp_split_branch fmt (phi, clo_phi) =
  fprintf fmt "@[<hv>%a =>@ %a@]" pp_cof phi pp_clo clo_phi

and pp_hd : hd Pp.printer =
  fun fmt ->
  function
  | Global sym ->
    fprintf fmt "global[%a]" Symbol.pp sym
  | Var lvl ->
    fprintf fmt "var[%i]" lvl
  | UnstableCut _ ->
    fprintf fmt "<unstable>"
  | Coe _ ->
    fprintf fmt "<coe>"

and pp_spine =
  fun fmt sp ->
  let comma fmt () = fprintf fmt ", " in
  pp_print_list ~pp_sep:comma pp_frame fmt sp

and pp_frame =
  fun fmt ->
  function
  | KAp (_, con) -> fprintf fmt "ap[%a]" pp_con con
  | KFst -> fprintf fmt "fst"
  | KSnd -> fprintf fmt "snd"
  | KProj lbl -> fprintf fmt "proj[%a]" pp_path lbl
  | KNatElim _ -> fprintf fmt "<nat-elim>"
  | KCircleElim _ -> fprintf fmt "<circle-elim>"
  | KElOut -> Uuseg_string.pp_utf_8 fmt "⭝ₑₗ"

and pp_cof : cof Pp.printer =
  fun fmt cof ->
  pp_con fmt @@ cof_to_con cof

and pp_dim : dim Pp.printer =
  fun fmt r ->
  pp_con fmt @@ dim_to_con r

and pp_clo : tm_clo Pp.printer =
  let sep fmt () = fprintf fmt "," in
  fun fmt (Clo (tm, {tpenv; conenv})) ->
    fprintf fmt "clo[%a ; [%a ; %a]]"
      S.dump tm
      (pp_list_group ~left:pp_lsq ~right:pp_rsq ~sep pp_tp) (Bwd.Bwd.to_list tpenv)
      (pp_list_group ~left:pp_lsq ~right:pp_rsq ~sep pp_con) (Bwd.Bwd.to_list conenv)

and pp_tp_clo : tp_clo Pp.printer =
  let sep fmt () = fprintf fmt "," in
  fun fmt (Clo (tp, {tpenv; conenv})) ->
    fprintf fmt "tpclo[%a ; [%a ; %a]]"
      S.dump_tp tp
      (pp_list_group ~left:pp_lsq ~right:pp_rsq ~sep pp_tp) (Bwd.Bwd.to_list tpenv)
      (pp_list_group ~left:pp_lsq ~right:pp_rsq ~sep pp_con) (Bwd.Bwd.to_list conenv)

and pp_sign_clo : (S.sign clo) Pp.printer =
  let sep fmt () = fprintf fmt "," in
  fun fmt (Clo (sign, {tpenv; conenv})) ->
    fprintf fmt "tpclo[%a ; [%a ; %a]]"
      S.dump_sign sign
      (pp_list_group ~left:pp_lsq ~right:pp_rsq ~sep pp_tp) (Bwd.Bwd.to_list tpenv)
      (pp_list_group ~left:pp_lsq ~right:pp_rsq ~sep pp_con) (Bwd.Bwd.to_list conenv)

and pp_con : con Pp.printer =
  fun fmt ->
  function
  | Cut {cut;tp} ->
    fprintf fmt "cut[%a :: %a]" pp_cut cut pp_tp tp
  | Zero ->
    fprintf fmt "zero"
  | Suc con ->
    fprintf fmt "suc[%a]" pp_con con
  | Base ->
    fprintf fmt "base"
  | Loop r ->
    fprintf fmt "loop[%a]" pp_dim r
  | Pair (con0, con1) ->
    fprintf fmt "pair[%a,%a]" pp_con con0 pp_con con1
  | Struct fields ->
    fprintf fmt "struct[%a]"
      (Pp.pp_sep_list (fun fmt (lbl, tp) -> fprintf fmt "%a : %a" pp_path lbl pp_con tp)) fields
  | Prf ->
    fprintf fmt "*"
  | Cof (Cof.Join phis) ->
    fprintf fmt "join[%a]" (Pp.pp_sep_list pp_con) phis
  | Cof (Cof.Meet phis) ->
    fprintf fmt "meet[%a]" (Pp.pp_sep_list pp_con) phis
  | Cof (Cof.Eq (r, s)) ->
    fprintf fmt "eq[%a,%a]" pp_con r pp_con s
  | DimProbe x ->
    fprintf fmt "probe[%a]" DimProbe.pp x
  | Lam (_, clo) ->
    fprintf fmt "lam[%a]" pp_clo clo
  | Dim0 ->
    fprintf fmt "dim0"
  | Dim1 ->
    fprintf fmt "dim1"
  | ElIn con ->
    fprintf fmt "el/in[%a]" pp_con con
  | StableCode `Nat ->
    fprintf fmt "nat/code"
  | StableCode `Circle ->
    fprintf fmt "circle/code"
  | SubIn _ ->
    fprintf fmt "<sub/in>"
  | FHCom _ ->
    fprintf fmt "<fhcom>"
  | LetSym _ ->
    fprintf fmt "<let-sym>"
  | StableCode `Univ -> fprintf fmt "<code-univ>"
  | BindSym _ -> fprintf fmt "<bind-sym>"
  | StableCode code -> pp_stable_code fmt code
  | UnstableCode _ -> fprintf fmt "<unstable-code>"
  | Box _ -> fprintf fmt "<box>"
  | VIn _ -> fprintf fmt "<vin>"
  | Split branches ->
    let sep fmt () = fprintf fmt "@ | " in
    pp_list_group ~left:pp_lsq ~right:pp_rsq ~sep
      pp_split_branch
      fmt
      branches
  | LockedPrfIn _ ->
    fprintf fmt "<wrap>"


and pp_sign fmt =
  function
  | Field (ident, tp, clo) -> fprintf fmt "sig/field[%a,%a,%a]" pp_path ident pp_tp tp pp_sign_clo clo
  | Empty -> fprintf fmt "sig/empty"

and pp_tp fmt =
  function
  | Pi (base, ident, fam) ->
    fprintf fmt "pi[%a,%a,%a]" pp_tp base Ident.pp ident pp_tp_clo fam
  | Sg _ ->
    fprintf fmt "<sg>"
  | Signature sign ->
    fprintf fmt "sig[%a]" pp_sign sign
  | Sub _ ->
    fprintf fmt "<sub>"
  | TpPrf _ ->
    fprintf fmt "<prf>"
  | TpCof ->
    fprintf fmt "<cof>"
  | TpDim ->
    fprintf fmt "<dim>"
  | Univ ->
    fprintf fmt "<univ>"
  | Nat ->
    fprintf fmt "<nat>"
  | Circle ->
    fprintf fmt "<circle>"
  | ElStable code ->
    fprintf fmt "el[%a]" pp_stable_code code
  | ElCut con ->
    fprintf fmt "el-cut[%a]" pp_cut con
  | ElUnstable (`HCom _) ->
    fprintf fmt "<Hcom>"
  | ElUnstable (`V _) ->
    fprintf fmt "<V>"
  | TpSplit _ ->
    fprintf fmt "<split>"
  | TpLockedPrf _ ->
    fprintf fmt "<wrap>"

and pp_stable_code fmt =
  function
  | `Ext _ -> fprintf fmt "<code-ext>"
  | `Pi _ -> fprintf fmt "<code-pi>"
  | `Sg _ -> fprintf fmt "<code-sg>"
  | `Signature _ -> fprintf fmt "<code-sig>"
  | `Nat -> fprintf fmt "<code-nat>"
  | `Circle -> fprintf fmt "<code-circle>"
  | `Univ -> fprintf fmt "<code-univ>"


