namespace Basis

module Void =
    type t = unit

    let abort : t -> 'a = function _ -> .
