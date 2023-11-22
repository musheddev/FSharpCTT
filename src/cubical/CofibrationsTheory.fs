namespace Cubical

open Basis
open Bwd


type cof = (Dim, int) Cof

//let dump_var fmt i = fprintf fmt "L[%i]" i

//let dump_cof = Cof.dump_cof Dim.dump dump_var


//type UF = DisjointSet<Dim,Set<int>>



(** A presentation of an algebraic theory over the language of intervals and cofibrations. *)
type alg_thy' =
  { classes : DisjointSet<Dim,Set<int>>;
    (** equivalence classes of dimensions *)

    true_vars : Set<int>
    
  }

type eq = Dim * Dim

(** A [branch] represents the meet of a bunch of atomic cofibrations. *)
type branch = Set<int> * eq list
type branches = branch list

(** A [cached_branch] is a [branch] together with an algebraic theory
  * representing the resulting theory at the end of the branch.  *)
type cached_branch = alg_thy' * branch
type cached_branches = cached_branch list

(** As an optimization, we remember when a theory is consistent or not. *)
type alg_thy =
  | Consistent of alg_thy'
  | Inconsistent 



(** A [branch] is consistent if it is consistent and all of its
  * sub-branches are consistent. *)
(** A disjoint theory is the join of a list of [cached_branch]. We do not need to
  * remember the common ancestor of these branches (as an algebraic theory), but only
  * the atomic cofibrations labeling the path from the common ancestor to each branch. *)
type disj_thy = cached_branches

(** This is to dissect the meet of a list of cofibrations into a list of branches.
  *
  * Possible further optimizations:
  * 1. Should we use [Cof.reduce] to massage the cofibrations first?
  * 2. Should we eagerly factor out common cofibrations to facilitate the refactoring
  *    steps later on? (This does not seem to be helpful in preliminary experiments.)
*)
module Dissect =
  let rec dissect_cofibrations : cof list -> branches =
    function
    | [] -> [Set.empty, []]
    | cof :: cofs ->
      match cof with
      | Cof.Var v ->
        List.map (fun (vars, eqs) -> Set.add v vars, eqs) <|
        dissect_cofibrations cofs
      | Cof.Cof cof ->
        match cof with
        | Cof_f.Meet meet_cofs ->
          dissect_cofibrations <| meet_cofs @ cofs
        | Cof_f.Join join_cofs ->
          List.concat (join_cofs |> List.map (fun join_cof ->
          dissect_cofibrations (join_cof :: cofs)))
        | Cof_f.Eq (r, s) ->
          List.map (fun (vars, eqs) -> vars, (r, s) :: eqs) <|
          dissect_cofibrations cofs

module Alg =

  type t = alg_thy
  type t' = alg_thy'

  let emp' =
    {classes = DisjointSet.empty;
     true_vars = Set.empty}

  let empty =
    Consistent emp'

  let consistency =
    function
    | Consistent _ -> true
    | Inconsistent -> false

  let assume_vars (thy : alg_thy') vars =
    {thy with true_vars = Set.union vars thy.true_vars}

  let test_eq (thy : alg_thy') (r, s) =
    DisjointSet.test r s thy.classes

  (** [unsafe_test_and_assume_eq] fuses [test_eq] and [assume_eq] (if there was one).
    * It is "unsafe" because we do not check consistency here. *)
  let unsafe_test_and_assume_eq (thy : alg_thy') (r, s) =
    let testing, classes = DisjointSet.test_and_union r s thy.classes in
    testing, {thy with classes}

  let test_eqs (thy : alg_thy') eqs =
    List.forall (test_eq thy) eqs

  let test_var (thy : alg_thy') v =
    Set.mem v thy.true_vars

  let test_vars (thy : alg_thy') vs =
    Set.isSubset vs thy.true_vars

  let test_branch (thy : alg_thy') (vars, eqs) =
    test_vars thy vars && test_eqs thy eqs

  (** [reduced_vars] takes out redundant cofibration variables. *)
  let reduce_vars (thy : alg_thy') vars =
    Set.difference vars thy.true_vars

  (** [reduce_eqs] detects inconsistency of an equation set and takes out
    * redundant equations. *)
  let reduce_eqs (thy : alg_thy') eqs =
    let go ((thy', eqs) as acc) eq =
      match unsafe_test_and_assume_eq thy' eq with
      | true, _ -> acc
      | false, thy' -> thy',  Snoc (eqs, eq)
    in
    let thy', eqs = List.fold_left go (thy, Emp) eqs in
    match test_eq thy' (Dim0, Dim1) with
    | true -> Inconsistent
    | false -> Consistent (thy', Bwd.to_list eqs)

  (** [reduce_branch] detects inconsistency of a branch and takes out redundant
    * cofibration variables and equations. *)
  let reduce_branch (thy' : alg_thy') (vars, eqs) =
    match reduce_eqs thy' eqs with
    | Inconsistent -> Inconsistent
    | Consistent (thy', eqs) ->
      Consistent (assume_vars thy' vars, (reduce_vars thy' vars, eqs))

  (** [reduce_branches] removes inconsistent branches and takes out redundant
    * cofibration variables and equations. *)
  let reduce_branches (thy' : alg_thy') branches : cached_branches =
    let go branch =
      match reduce_branch thy' branch with
      | Inconsistent -> None
      | Consistent (thy', branch) -> Some (thy', branch)
    in
    List.filter_map go branches

  (** [drop_useless_branches] drops the branches that could be dropped without
    * affecting the coverages. *)
  let drop_useless_branches cached_branches : cached_branches =
    let go_fwd acc (thy', branch) =
      if Bwd.exists (fun (_, branch) -> test_branch thy' branch) acc then
        acc
      else
        Snoc (acc, (thy', branch))
    in
    let cached_branches = List.fold_left go_fwd Emp cached_branches in
    let go_bwd (thy', branch) acc =
      if List.exists (fun (_, branch) -> test_branch thy' branch) acc then
        acc
      else
        (thy', branch) :: acc
    in
    Bwd.fold_right go_bwd cached_branches []

  (** [split] combines all the optimizers above to split an algebraic theory
    * into multiple ones induced by the input cofibration context. *)
  let split (thy : alg_thy) (cofs : cof list) : t list =
    match thy with
    | Inconsistent -> []
    | Consistent thy' ->
      match dissect_cofibrations cofs with
      | [] -> []
      | [vars, []] when Set.isEmpty vars -> [Consistent thy']
      | dissected_cofs ->
        begin
          drop_useless_branches <|
          reduce_branches thy' dissected_cofs
        end |> List.map <| fun (thy', _) -> Consistent thy'

  (** [test] checks whether a cofibration is true within an algebraic theory *)
  let rec test (thy' : alg_thy') : cof -> bool =
    function
    | Cof.Cof phi ->
      begin
        match phi with
        | Cof_f.Eq (r, s) ->
          test_eq thy' (r, s)
        | Cof_f.Join phis ->
          List.exists (test thy') phis
        | Cof_f.Meet phis ->
          List.forall (test thy') phis
      end
    | Cof.Var v ->
      test_var thy' v

  let left_invert_under_cofs ~zero ~seq (thy : alg_thy) cofs cont =
    match split thy cofs with
    | [] -> zero
    | [thy] -> cont thy
    | thys -> seq cont thys


module Disj =


  let envelop_alg' alg_thy' : disj_thy =
    [alg_thy', (Set
    .empty, [])]

  let envelope_alg =
    function
    | Consistent alg_thy' -> envelop_alg' alg_thy'
    | Inconsistent -> []


  let empty : disj_thy = [Alg.emp', (Set
  .empty, [])]

  let consistency =
    function
    | [] -> false
    | _ -> true

  (** [refactor_branches] attempts to identify common parts of the branches
    * and shrink the labels. Recall that we do not keep the common ancestor
    * but the paths (as a collection of atomic cofibrations) from the common
    * ancestor, and thus what are changed here are the paths.
    *
    * This optimization seems to be expensive, but it seems to help after
    * we switched from the persistant tables (using [Hashtbl]) to [Map].
  *)
  let refactor_branches cached_branches : disj_thy =
    let common_vars =
      let go vars0 (_, (vars1, _)) = Set.inter vars0 vars1 in
      match cached_branches with
      | [] -> Set.empty
      | (_, (vars, _)) :: branches -> List.fold_left go vars branches
    
    (* The following is an approximation of checking whether some equation is useful.
     * It does not kill every "useless" cofibration. Here is one example:
     *
     * branch 1: r=0
     * branch 2: r=i, i=0
     *
     * r=0 will be factored out, but then i=0 should also be removed. Here is a more
     * complicated example:
     *
     * branch 1: r=i, i=0
     * branch 2: r=j, j=0
     *
     * Both i=0 and j=0 should be factored out, but the following code is not smart
     * enough to detect them. One could consider more aggressive approaches if [CofThy]
     * becomes the bottleneck again.
    *)
    let useful eq = cached_branches |> List.exists (fun (thy', _) -> not <| Alg.test_eq thy' eq)
    (* revisit all branches and remove all useless ones identified by the simple criterion above. *)
    cached_branches |> List.map (fun (thy', (vars, eqs)) ->
    thy', (Set.diff vars common_vars, List.filter useful eqs))

  (** [split thy cofs] adds to the theory [thy] the conjunction of a list of cofibrations [cofs]
    * and calculate the branches accordingly. This is similar to [Alg.split] in the spirit but
    * differs in detail. *)
  let split (thy : alg_thy) (cofs : cof list) : disj_thy =
    match dissect_cofibrations cofs with
    | [] -> []
    | [vars, []] when Set.isEmpty vars -> thy
    | dissected_cofs ->
      Alg.drop_useless_branches 
        thy |> List.concat_map (fun (thy', (vars, eq)) ->
        Alg.reduce_branches thy' dissected_cofs |> List.map (fun (thy', (sub_vars, sub_eqs)) ->
        thy', (Set.union vars sub_vars, eq @ sub_eqs)))
      

  (** [assume thy cofs] is the same as [split thy cofs] except that it further refactors the
    * branches to optimize future searching. *)
  let assume (thy : alg_thy) (cofs : cof list) : disj_thy =
    refactor_branches <| split thy cofs

  let test_sequent thy cx cof =
    split thy cx |> List.map (fun (thy', _) -> thy') |> List.forall (fun thy' -> Alg.test thy' cof)

  let left_invert zero seq thy cont =
    match thy |> List.map (fun (thy', _) -> Consistent thy') with
    | [] -> zero
    | [thy'] -> cont thy'
    | thy's -> seq cont thy's

