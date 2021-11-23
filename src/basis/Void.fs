namespace Basis

module Void =
    
    let abort : unit -> 'a = function _ -> Unchecked.defaultof<'a>
