module Core.DomainData
open Basis
open Cubical
open Bwd
open Core.Syntax

type dim = Dim
type cof = (dim, int) Cof

(** A type code whose head constructor is stable under dimension substitution. *)
type stable_code<'a> =
  | Pi of 'a * 'a
  (** Dependent product type *)

  | Sg of 'a * 'a
  (** Dependent sum type *)

  | Signature of (string list * 'a) list
  (** First-Class Record types *)

  | Ext of int * 'a * 'a[] * 'a
  (** Extension type *)

  | Nat
  (** Natural numbers type *)

  | Circle
  (** The circle [S1]. *)

  | Univ
    (** A code for the universe (antinomous for now). *)
  

(** A type code whose head constructor is {i not} stable under dimension substitution. *)
type 'a unstable_code =
  | HCom of dim * dim * cof * 'a
  (** Formal composite types *)

  | V of dim * 'a * 'a * 'a
    (** V types, for univalence *)
  

type env<'a> = {tpenv : tp<'a> bwd; conenv: con<'a> bwd}

(** A {i closure} combines a semantic environment with a syntactic object binding an additional variable. *)
and 'a clo = Closure of ('a * env<'a>)
and 'a tp_clo = clo<Syntax.tp<'a>>
and 'a tm_clo = clo<Syntax.SyntaxData<'a>>
and 'a sign_clo = clo<Syntax.sign<'a>>


and [<RequireQualifiedAccess>] FHCom =
  | Nat 
  | Circle

(** Value constructors are governed by {!type:con}; we do not maintain in the datatype {i a priori} any invariant that these represent whnfs (weak head normal forms). Whether a value constructor is a whnf is contingent on the ambient local state, such as the cofibration theory. *)
and con<'a> =
  | Lam of Ident * tm_clo<'a>

  | BindSym of DimProbe * con<'a>
  (** A nominal binder of a dimension; these are used during the execution of coercion, which must probe a line of type codes with a fresh dimension. *)

  | LetSym of dim * DimProbe * con<'a>
  (** An explicit substitution of a dimension for a symbol. *)

  | Cut of tp<'a> * cut<'a>
  (** Our notion of {i neutral} value, a type annotated {!type:cut}. *)

  | Zero
  | Suc of con<'a>
  | Base
  | Loop of dim
  | Pair of con<'a> * con<'a>
  | Struct of (string list * con<'a>) list
  | SubIn of con<'a>

  | ElIn of con<'a>
  (** The introduction form for the extension of a {i stable} type code only (see {!constructor:ElStable}). *)

  | Dim0
  | Dim1
  | DimProbe of DimProbe

  | Cof of  Cof_f<con<'a>, con<'a>>
  (** A mixin of the language of cofibrations (as described in {!module:Cubical.Cof}), with dimensions and indeterminates in {!type:con}. *)

  | Prf

  | FHCom of FHCom * dim * dim * cof * con<'a>

  | StableCode of con<'a> stable_code
  | UnstableCode of con<'a> unstable_code

  | Box of dim * dim * cof * con<'a> * con<'a>
  | VIn of dim * con<'a> * con<'a> * con<'a>

  | Split of (cof * tm_clo<'a>) list

  | LockedPrfIn of con<'a>

and tp<'a> =
  | Sub of tp<'a> * cof * tm_clo<'a>
  | Univ
  | ElCut of cut<'a>
  | ElStable of con<'a> stable_code
  | ElUnstable of con<'a> unstable_code
  | TpDim
  | TpCof
  | TpPrf of cof
  | TpSplit of (cof * tp_clo<'a>) list
  | Pi of tp<'a> * Ident * tp_clo<'a>
  | Sg of tp<'a> * Ident * tp_clo<'a>
  | Signature of sign<'a>
  | Nat
  | Circle
  | TpLockedPrf of cof

and sign<'a> =
  | Field of string list * tp<'a> * clo<Syntax.sign<'a>>
  | Empty

(** A head is a variable (e.g. {!constructor:Global}, {!constructor:Var}), or it is some kind of unstable elimination form ({!constructor:Coe}, {!constructor:UnstableCut}). The geometry of {!type:cut}, {!type:hd}, {!type:unstable_frm} enables a very direct way to re-reduce a complex cut to whnf by following the unstable nodes to the root. *)
and hd<'a> =
  | Global of Global
  (** A top-level declaration*)

  | Var of int
  (** De Bruijn level *)

  | Coe of con<'a> * dim * dim * con<'a>
  | UnstableCut of cut<'a> * unstable_frm<'a>

(** A {!type:cut} is a value that is blocked on the computation of a {!type:hd} ("head"); when the head is computed, the list of stack frames ({!type:frm}) carried by the cut will be enacted. *)
and cut<'a> = hd<'a> * frm<'a> list

(** A {i stable} frame is a {i dimension substitution-stable} elimination form with a hole in place of its principal argument. Unstable elimination forms are governed by {!type:hd} to ease the "re-reduction" of a value to whnf under a stronger cofibration theory. *)
and frm<'a> =
  | KAp of tp<'a> * con<'a>
  | KFst
  | KSnd
  | KProj of string list
  | KNatElim of con<'a> * con<'a> * con<'a>
  | KCircleElim of con<'a> * con<'a> * con<'a>

  | KElOut
  (** The elimination form for the extension of a {i stable} type code only (see {!constructor:ElStable}). *)


(** An {i unstable} frame is a {i dimension substitution-unstable} elimination form with a hole in place of its principal argument. *)
and unstable_frm<'a> =
  | KHCom of dim * dim * cof * con<'a>
  | KCap of dim * dim * cof * con<'a>
  | KVProj of dim * con<'a> * con<'a> * con<'a>
  | KSubOut of cof * tm_clo<'a>
  | KLockedPrfUnlock of tp<'a> * cof * con<'a>

