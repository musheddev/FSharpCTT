namespace Basis
//open Lexing
open System
open System.IO

module LexingUtil =

  type span =
    {start : int64 * int64// Lexing.position;
     stop : int64 * int64 //Lexing.position
     file_name : string
    }

  let pp_span =
    fun fmt span ->
      fprintf fmt "%s:%i.%i-%i.%i"
        (* HACK: We use the basename, rather than the full path here
          to avoid issues with the test suite. This is bad, and should
          be changed once more thought is put into how we want to
          handle fancier imports/project structures. *)
        (Path.GetFileName span.file_name)
        (fst span.start)
        (snd span.start)
        (fst span.stop)
        (snd span.stop)

  let last_token (lexbuf : Stream) = 
    let tok = lexeme lexbuf in
    if tok = "" then None else Some tok

  let current_span (lexbuf : Stream) = 
    {start = lexbuf.lex_start_p; stop = lexbuf.lex_curr_p}
