namespace Core

[<AutoOpen>]
module Debug =
  let debug_enabled = ref true

  let debug_mode is_debug =
    debug_enabled := is_debug

  let is_debug_mode () =
    !debug_enabled

  // let debug_formatter =
  //   System.Console.Out
  //   let out buf pos len =
  //     if !debug_enabled then
  //       Stdlib.output_substring Stdlib.stderr buf pos len
  //     else
  //       ()
  //   in
  //   let flush () = Stdlib.flush Stdlib.stderr in
  //   Format.make_formatter out flush

  let print (fmt : Printf.TextWriterFormat<'T>) =
    if !debug_enabled then
      let debug_formatter = System.Console.Out
      fprintf debug_formatter "[DEBUG] ";
      fprintf debug_formatter fmt
