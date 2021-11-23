namespace Basis

module DisjointSet =
  //type T = PersistentTable.M (O)

  //type key = O.t
  type t =
    {rank : Map<int,'a>;
     parent : key T.t}

  let empty =
    {rank = PersistentTable.empty;
     parent = PersistentTable.empty}


  let find (x : key) (h : t) =
    let rec loop x p =
      match
        T.get_opt x p
      with
      | Some x -> loop x p
      | None -> x
    in
    loop x h.parent

  let get_rank cx h =
    match
      T.get_opt cx h.rank
    with
    | Some r -> r
    | _ -> 0

  let test (x : key) (y : key) (h : t) =
    x = y ||
    let cx = find x h in
    let cy = find y h in
    cx = cy

  let test_and_union (x : key) (y : key) (h : t) =
    if x = y then
      true, h
    else
      let cx = find x h in
      let cy = find y h in
      if cx = cy then
        true, h
      else
        false,
        begin
          let rx = get_rank cx h in
          let ry = get_rank cy h in
          if rx > ry then
            {h with
             parent = T.set cy cx h.parent}
          else if rx < ry then
            {h with
             parent = T.set cx cy h.parent}
          else
            {rank = T.set cx (rx + 1) h.rank;
             parent = T.set cy cx h.parent}
        end

  let union (x : key) (y : key) (h : t) =
    let cx = find x h in
    let cy = find y h in
    if cx != cy then
      begin
        let rx = get_rank cx h in
        let ry = get_rank cy h in
        if rx > ry then
          {h with
           parent = T.set cy cx h.parent}
        else if rx < ry then
          {h with
           parent = T.set cx cy h.parent}
        else
          {rank = T.set cx (rx + 1) h.rank;
           parent = T.set cy cx h.parent}
      end
    else
      h
end
