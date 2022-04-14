namespace Core
open System
type SyntaxPrecedence = int * int

module SyntaxPrecedence =

  let nonassoc n = 2*n, 2*n
  let left n = 2*n, 2*n+1
  let right n = 2*n+1, 2*n
  let prefix n = Int32.MaxValue, 2*n
  let postfix n = 2*n, Int32.MaxValue

  let dual (l, _) (_, r) = l, r

  let pp fmt (l, r) = fprintf fmt "<%i-%i>" l r

  type env = int * int
  let left_of (l, _) = Int32.MinValue, l
  let right_of (_, r) = r, Int32.MinValue
  let surrounded_by (l, r) = r, l
  let isolated = Int32.MinValue, Int32.MinValue
  let isolate_left (_, r) = Int32.MinValue, r
  let isolate_right (l, _) = l, Int32.MinValue

  let pp_env fmt (l,r) =
    match l = Int32.MinValue, r = Int32.MinValue with
    | true, true -> fprintf fmt "<none>"
    | false, true -> fprintf fmt "<left:%i>" l
    | true, false -> fprintf fmt "<right:%i>" r
    | false, false -> fprintf fmt "<dual:%i;%i>" l r

  let parens (l', r') (l, r) = l' >= l || r' >= r
