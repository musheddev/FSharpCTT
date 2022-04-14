namespace Basis

type Symbol = 

  val compare : Symbol -> Symbol -> int
  val equal : Symbol -> Symbol -> bool

  val pp : unit //pretty printer
  val show : Symbol -> string

  // val serialize : S -> J.value
  // val deserialize : J.value -> S

