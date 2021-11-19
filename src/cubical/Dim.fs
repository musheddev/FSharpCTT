namespace Cubical

type Dim =
  | Dim0
  | Dim1
  | DimVar of int
  | DimProbe of DimProbe

with
static member dump fmt =
  function
  | Dim0 -> fprintf fmt "dim0"
  | Dim1 -> fprintf fmt "dim1"
  | DimVar i -> fprintf fmt "dim#var[%i]" i
  | DimProbe sym -> fprintf fmt "dim#probe[%a]" DimProbe.pp sym
