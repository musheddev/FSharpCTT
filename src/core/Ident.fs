namespace Core
type Ident =
    | Anon
    | User of string list
    | Machine of string

with
  static member qual_to_string =
    function
    | [] -> "(root)"
    | parts -> String.concat "." parts

  static member pp fmt =
    function
    | Anon -> fprintf fmt "<anon>"
    | User parts -> Uuseg_string.pp_utf_8 fmt (qual_to_string parts)
    | Machine str -> Uuseg_string.pp_utf_8 fmt str

  static member to_string =
    function
    | Anon -> "<anon>"
    | User parts -> Ident.qual_to_string parts
    | Machine str -> str

  static member to_string_opt =
    function
    | User parts -> Some (Ident.qual_to_string parts)
    | Machine nm -> Some nm
    | Anon -> None
