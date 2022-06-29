namespace Basis

type DisjointSet<'key,'a> when 'key : comparison =
    {rank : Map<int,'a> ;
     parent : Map<'key,'a> }

module DisjointSet =
  //type T = PersistentTable.M (O)

  //type key = O.t
  // type t<'key,'a> =
  //   {rank : Map<int,'a>;
  //    parent : Map<'key,'a>}

  let empty =
    {rank = PersistentTable.empty;
     parent = PersistentTable.empty}


  let find (x : 'key) (h : 't) =
    let rec loop x p =
      match (PersistentTable.get_opt x p) with
      | Some x -> loop x p
      | None -> x
    loop x h.parent

  let get_rank (cx : 'key) h =
    match PersistentTable.get_opt cx h.rank with
    | Some r -> r
    | _ -> 0

  let test (x : 'key) (y : 'key) (h : 't) =
    x = y ||
    let cx = find x h in
    let cy = find y h in
    cx = cy

  let test_and_union (x : 'key) (y : 'key) (h : 't) =
    if x = y then
      true, h
    else
      let cx = find x h
      let cy = find y h
      if cx = cy then
        true, h
      else
        false,
        
          let rx = get_rank cx h
          let ry = get_rank cy h
          if rx > ry then
            {h with
             parent = PersistentTable.set cy cx h.parent}
          else if rx < ry then
            {h with
             parent = PersistentTable.set cx cy h.parent}
          else
            {rank = PersistentTable.set cx (rx + 1) h.rank;
             parent = PersistentTable.set cy cx h.parent}
        

  let union (x : 'key) (y : 'key) (h : 't) =
    let cx = find x h
    let cy = find y h
    if cx <> cy then
      
        let rx = get_rank cx h
        let ry = get_rank cy h
        if rx > ry then
          {h with
           parent = PersistentTable.set cy cx h.parent}
        else if rx < ry then
          {h with
           parent = PersistentTable.set cx cy h.parent}
        else
          {rank = PersistentTable.set cx (rx + 1) h.rank;
           parent = PersistentTable.set cy cx h.parent}
      
    else
      h

