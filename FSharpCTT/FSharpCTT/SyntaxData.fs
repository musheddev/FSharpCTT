namespace Core
//open Basis
open Cubical

module SyntaxData =

  type Ident =
    | Anon
    | User of string list
    | Machine of string
  
  type Global =
    { index : int;
      name : string option }
    
  with
    static member compare (s1 : Global) (s2 : Global) = s1.index.CompareTo(s2.index)

    static member equal s1 s2 =
      s1.index = s2.index
  
  type term<'s> =
    | Var of int
    | Global of Global
    | Let of term<'s> * Ident * term<'s>
    | Ann of term<'s> * tp<'s>

    | Zero
    | Suc of term<'s>
    | NatElim of term<'s> * term<'s> * term<'s> * term<'s>

    | Base
    | Loop of term<'s>
    | CircleElim of term<'s> * term<'s> * term<'s> * term<'s>

    | Lam of Ident * term<'s>
    | Ap of term<'s> * term<'s>

    | Pair of term<'s> * term<'s>
    | Fst of term<'s>
    | Snd of term<'s>

    | Struct of (string list * term<'s>) list
    | Proj of term<'s> * string list

    | Coe of term<'s> * term<'s> * term<'s> * term<'s>
    | HCom of term<'s> * term<'s> * term<'s> * term<'s> * term<'s>
    | Com of term<'s> * term<'s> * term<'s> * term<'s> * term<'s>

    | SubIn of term<'s>
    | SubOut of term<'s>

    | Dim0
    | Dim1
    | Cof of Cof_f<term<'s>, term<'s>>
    | ForallCof of term<'s>
    | CofSplit of (term<'s> * term<'s>) list
    | Prf

    | ElIn of term<'s>
    | ElOut of term<'s>

    | Box of term<'s> * term<'s> * term<'s> * term<'s> * term<'s>
    | Cap of term<'s> * term<'s> * term<'s> * term<'s> * term<'s>

    | VIn of term<'s> * term<'s> * term<'s> * term<'s>
    | VProj of term<'s> * term<'s> * term<'s> * term<'s> * term<'s>

    | CodeExt of int * term<'s> * 's * term<'s>
    | CodePi of term<'s> * term<'s>
    | CodeSg of term<'s> * term<'s>
    | CodeSignature of (string list * term<'s>) list
    | CodeNat
    | CodeUniv
    | CodeV of term<'s> * term<'s> * term<'s> * term<'s>
    | CodeCircle

    | ESub of sub<'s> * term<'s>
    (** Explicit substition *)

    | LockedPrfIn of term<'s>
    | LockedPrfUnlock of tp : tp<'s> * cof : term<'s> * prf : term<'s> * bdy : term<'s>

  and tp<'s> =
    | Univ
    | El of term<'s>
    | TpVar of int
    | TpDim
    | TpCof
    | TpPrf of term<'s>
    | TpCofSplit of (term<'s> * tp<'s>) list
    | Sub of tp<'s> * term<'s> * term<'s>
    | Pi of tp<'s> * Ident * tp<'s>
    | Sg of tp<'s> * Ident * tp<'s>
    | Signature of sign<'s>
    | Nat
    | Circle
    | TpESub of sub<'s> * tp<'s>
    | TpLockedPrf of term<'s>

  and sign<'s> = (string list * tp<'s>) list

  (** The language of substitions from {{:https://arxiv.org/abs/1102.2405} Abel, Coquand, and Pagano}. *)
  and sub<'s> =
    | SbI
    (** The identity substitution [Γ → Γ]. *)

    | SbC of sub<'s> * sub<'s>
    (** The composition of substitutions [δ ∘ γ]. *)

    | Sb1
    (** The terminal substitution [Γ → 1]. *)

    | SbE of sub<'s> * term<'s>
    (** The universal substitution into an extended context [<γ, a>]. *)

    | SbP
    (** The projection from a extended context [Γ.A → Γ]. *)

