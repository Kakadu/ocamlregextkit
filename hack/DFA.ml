module type STATE = sig
  include Hashtbl.HashedType

  val gen_state : unit -> t
end

module type S = sig
  type t

  val intersection : t -> t -> t
end

module Mark = struct
  type halfmark = int

  type t =
    { min : halfmark
    ; max : halfmark
    }

  let compare = Stdlib.compare
end

module Mark_multimap = struct
  include Map.Make (Mark)
end

module Make (State : STATE) : S = struct
  (*  *)
  module State_hash = Hashtbl.Make (State)

  module State_pair = struct
    type t = State.t * State.t * State.t

    let equal (a, b, c) (a1, b1, c1) = State.(equal a a1 && equal b b1 && equal c c1)
    let hash (a, b, c) = State.hash a lxor State.hash b lxor State.hash c
    let make a b c = a, b, c
    let left (_, a, _) = a
    let right (_, _, a) = a
    let prod (a, _, _) = a
  end

  module State_pair_hash = Hashtbl.Make (State_pair)

  type states = unit State_hash.t
  type transitions = (Mark.t * State.t) State_hash.t

  type t =
    { mutable transitions : transitions
    ; initial : State.t
    ; states : states
    ; accepted : unit State_hash.t
    }

  let get_sorted_transitions : t -> _ = fun _ -> assert false

  let intersection a1 a2 =
    (* let trans1 = get_sorted_transitions a1 in *)
    (* let trans2 = get_sorted_transitions a2 in *)
    let new_states = State_pair_hash.create 42 in
    let init_work_list =
      let p = State_pair.make (State.gen_state ()) a1.initial a2.initial in
      State_pair_hash.add new_states p p;
      [ p ]
    in
    let new_accepted = State_hash.create 42 in
    let work_list = ref init_work_list in
    let rec loop () =
      match !work_list with
      | [] -> ()
      | p :: tl ->
        if State_hash.(
             mem a1.accepted (State_pair.left p) && mem a2.accepted (State_pair.right p))
        then State_hash.add new_accepted (State_pair.prod p) ();
        let t1 = State_hash.find_all a1.transitions (State_pair.left p) in
        let t2 = State_hash.find_all a2.transitions (State_pair.right p) in
        let t2len = List.length t2 in
        let get_max tr idx = (fst @@ List.nth tr idx).max in
        let get_min tr idx = (fst @@ List.nth tr idx).min in
        let n1 = ref 0 in
        let b2 = ref 0 in
        ListLabels.iter t1 ~f:(fun _ ->
          while
            !b2 < t2len
            && (get_max t2 !b2) < (get_min  t1 n1)
          do
            incr b2
          done;
          let rec loop2 n2 =
            if not (n2 < List.length t2 && )

            () in
          loop !b2);
        assert false
    in
    loop ();
    assert false
  ;;
end
