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
    | User parts -> fprintf fmt "%s" (Ident.qual_to_string parts)
    | Machine str -> fprintf fmt "%s" str

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
