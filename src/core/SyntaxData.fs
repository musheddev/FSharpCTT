namespace Core
open Basis
open Cubical



type SyntaxData<'s> =
  | Var of int
  | Global of 's
  | Let of SyntaxData<'s> * Ident * SyntaxData<'s>
  | Ann of SyntaxData<'s> * tp<'s>

  | Zero
  | Suc of SyntaxData<'s>
  | NatElim of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>

  | Base
  | Loop of SyntaxData<'s>
  | CircleElim of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>

  | Lam of Ident * SyntaxData<'s>
  | Ap of SyntaxData<'s> * SyntaxData<'s>

  | Pair of SyntaxData<'s> * SyntaxData<'s>
  | Fst of SyntaxData<'s>
  | Snd of SyntaxData<'s>

  | Struct of (string list * SyntaxData<'s>) list
  | Proj of SyntaxData<'s> * string list

  | Coe of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>
  | HCom of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>
  | Com of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>

  | SubIn of SyntaxData<'s>
  | SubOut of SyntaxData<'s>

  | Dim0
  | Dim1
  | Cof of (SyntaxData<'s>, SyntaxData<'s>) Cof_f
  | ForallCof of SyntaxData<'s>
  | CofSplit of (SyntaxData<'s> * SyntaxData<'s>) list
  | Prf

  | ElIn of SyntaxData<'s>
  | ElOut of SyntaxData<'s>

  | Box of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>
  | Cap of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>

  | VIn of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>
  | VProj of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>

  | CodeExt of int * SyntaxData<'s> * 's * SyntaxData<'s>
  | CodePi of SyntaxData<'s> * SyntaxData<'s>
  | CodeSg of SyntaxData<'s> * SyntaxData<'s>
  | CodeSignature of (string list * SyntaxData<'s>) list
  | CodeNat
  | CodeUniv
  | CodeV of SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s> * SyntaxData<'s>
  | CodeCircle

  | ESub of sub<'s> * SyntaxData<'s>
  (** Explicit substition *)

  | LockedPrfIn of SyntaxData<'s>
  | LockedPrfUnlock of tp : tp<'s> * cof : SyntaxData<'s> * prf : SyntaxData<'s> * bdy : SyntaxData<'s>

and tp<'s> =
  | Univ
  | El of SyntaxData<'s>
  | TpVar of int
  | TpDim
  | TpCof
  | TpPrf of SyntaxData<'s>
  | TpCofSplit of (SyntaxData<'s> * tp<'s>) list
  | Sub of tp<'s> * SyntaxData<'s> * SyntaxData<'s>
  | Pi of tp<'s> * Ident * tp<'s>
  | Sg of tp<'s> * Ident * tp<'s>
  | Signature of sign<'s>
  | Nat
  | Circle
  | TpESub of sub<'s> * tp<'s>
  | TpLockedPrf of SyntaxData<'s>

and sign<'s> = (string list * tp<'s>) list

(** The language of substitions from {{:https://arxiv.org/abs/1102.2405} Abel, Coquand, and Pagano}. *)
and sub<'s> =
  | SbI
  (** The identity substitution [Γ → Γ]. *)

  | SbC of sub<'s> * sub<'s>
  (** The composition of substitutions [δ ∘ γ]. *)

  | Sb1
  (** The terminal substitution [Γ → 1]. *)

  | SbE of sub<'s> * SyntaxData<'s>
  (** The universal substitution into an extended context [<γ, a>]. *)

  | SbP
  (** The projection from a extended context [Γ.A → Γ]. *)

