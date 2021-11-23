namespace Basis

type S = 

  val compare : S -> S -> int
  val equal : S -> S -> bool

  val pp : unit //pretty printer
  val show : S -> string

  // val serialize : S -> J.value
  // val deserialize : J.value -> S

