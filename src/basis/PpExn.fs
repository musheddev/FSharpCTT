namespace Basis

open System.Collections.Generic

exception Unrecognized

// module PpExn =
//   exception Break
//   let printers = Stack.create ()

//   let install_printer printer =
//     Stack.push printer printers;
//     Printexc.register_printer @@ fun exn ->
//     try
//       printer str_formatter exn;
//       Some (flush_str_formatter ())
//     with
//     | Unrecognized ->
//       None

//   let pp fmt exn =
//     let go printer =
//       try
//         printer fmt exn;
//         raise Break
//       with
//       | Unrecognized -> ()
//     in
//     try
//       Stack.iter go printers;
//       fprintf fmt "%s" @@ Printexc.to_string exn
//     with
//     | Break -> ()