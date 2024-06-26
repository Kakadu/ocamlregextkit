module SS = struct
  include Set.Make (String)

  let fold_left f acc x = fold (Fun.flip f) x acc
  let add_list xs ss = List.fold_left (Fun.flip add) ss xs
  let map_list f ss = fold (fun x acc -> f x :: acc) ss []

  let filter_map_list f ss =
    fold
      (fun x acc ->
        match f x with
        | Some x -> x :: acc
        | None -> acc)
      ss
      []
  ;;
end

type alphabet = SS.t

(*
   type 't automata =
  | A :
      { mutable states : ('t, 'cmp) Base.Set.t
      ; mutable alphabet : SS.t
      ; transitions : ('t, (string, 't) Hashtbl.t) Hashtbl.t
      ; mutable start : 't
      ; accepting : ('t, bool) Hashtbl.t
      }
      -> 't automata
*)
type 't automata =
  { mutable states : 't list
  ; mutable alphabet : SS.t
  ; transitions : ('t, (string, 't) Hashtbl.t) Hashtbl.t
  ; mutable start : 't
  ; accepting : ('t, bool) Hashtbl.t
  }

let map f from =
  let states = List.map f from.states in
  let start = f from.start in
  let transitions = Hashtbl.create (Hashtbl.length from.transitions) in
  Hashtbl.iter
    (fun k v ->
      let newk = f k in
      let newv = Hashtbl.create (Hashtbl.length v) in
      Hashtbl.iter (fun k v -> Hashtbl.add newv k (f v)) v;
      Hashtbl.add transitions newk newv)
    from.transitions;
  let accepting = Hashtbl.create (Hashtbl.length from.accepting) in
  Hashtbl.iter (fun k v -> Hashtbl.add accepting (f k) v) from.accepting;
  { states; start; transitions; alphabet = from.alphabet; accepting }
;;

let get_states m = m.states
let set_states m qs = m.states <- qs
let iter_states f m = List.iter f m.states
let filter_states f m = List.filter f m.states
let exists_states f m = List.exists f m.states
let for_all_states f m = List.for_all f m.states
let map_states f m = List.map f m.states
let get_alphabet m = m.alphabet
let set_alphabet m alph = m.alphabet <- alph
let iter_alphabet f m = SS.iter f m.alphabet

(* let filter_alphabet f m = SS.filter f m.alphabet *)
(* let map_alphabet f m = SS.map f m.alphabet *)
let exists_alphabet f m = SS.exists f m.alphabet
let for_all_alphabet f m = SS.for_all f m.alphabet

let get_transitions m =
  Hashtbl.fold
    (fun s ats acc -> Hashtbl.fold (fun a t acc' -> (s, a, t) :: acc') ats acc)
    m.transitions
    []
;;

let iter_transitions f m =
  Hashtbl.iter (fun s v -> Hashtbl.iter (fun a t -> f (s, a, t)) v) m.transitions
;;

let map_transitions f m =
  Hashtbl.fold
    (fun s v acc -> Hashtbl.fold (fun a t acc' -> f (s, a, t) :: acc') v acc)
    m.transitions
    []
;;

let get_start m = m.start

let get_accepting m =
  Hashtbl.fold (fun k v acc -> if v then k :: acc else acc) m.accepting []
;;

let iter_accepting f m = Hashtbl.iter (fun s v -> if v then f s) m.accepting

let map_accepting f m =
  Hashtbl.fold (fun s v acc -> if v then f s :: acc else acc) m.accepting []
;;

let get_next_states m s a =
  match Hashtbl.find_all (Hashtbl.find m.transitions s) a with
  | l -> l
  | exception Not_found -> []

let get_next_state m s a = Hashtbl.find (Hashtbl.find m.transitions s) a

let get_transitions_of_state m s =
  Hashtbl.fold
    (fun mark next_s acc -> (mark, next_s) :: acc)
    (Hashtbl.find m.transitions s)
    []
;;

let get_prev_states m t a =
  Hashtbl.fold
    (fun s v acc ->
      Hashtbl.fold (fun a' t' acc' -> if a = a' && t = t' then s :: acc' else acc') v acc)
    m.transitions
    []
;;

let is_accepting m s =
  match Hashtbl.find_opt m.accepting s with
  | None -> false
  | Some res -> res
;;

let get_reachable_states : 'a. 'a automata -> 'a list =
  fun m ->
  let rec find_reachable_states : _ list -> _ list -> _ list =
    fun to_visit visited ->
      match to_visit with
      | [] -> visited
      | st :: to_visit ->
        let visited = st :: visited in
        let to_visit =
          SS.fold_left (fun acc a ->
            List.fold_left (fun acc st -> if List.mem st visited then acc else st :: acc)
            acc (get_next_states m st a))
          to_visit (SS.add "ε" m.alphabet)
        in
        find_reachable_states to_visit visited
  in
  find_reachable_states [ m.start ] []
;;

let find_reachable_state (type t): (module Set.S with type elt = t) -> (t -> bool) -> t automata -> t =
  fun (module S) f m ->
  let rec find_reachable_state : t list -> S.t -> t =
    fun to_visit visited ->
    match to_visit with
    | [] -> raise Not_found
    | st :: _ when f st -> st
    | st :: to_visit ->
    let visited = S.add st visited in
    let to_visit =
      SS.fold_left
        (fun acc a ->
          List.fold_left
          (fun acc st -> if S.mem st visited then acc else st :: acc)
          acc (get_next_states m st a))
        to_visit
        (SS.add "ε" m.alphabet)
    in
    find_reachable_state to_visit visited
  in
  find_reachable_state [ m.start ] S.empty
;;

let filter_states_inplace m f =
  set_states m (List.filter f m.states);
  Hashtbl.filter_map_inplace (fun s ts -> if f s then Some ts else None) m.transitions;
  Hashtbl.filter_map_inplace (fun s b -> if f s then Some b else None) m.accepting
;;

let merge_states_inplace m p q =
  filter_states_inplace m (fun s -> s <> q);
  Hashtbl.iter
    (fun _ v -> Hashtbl.filter_map_inplace (fun _ t -> Some (if t = q then p else t)) v)
    m.transitions;
  if m.start = q then m.start <- p
;;

let add_to_alphabet m alph = set_alphabet m (SS.union m.alphabet alph)

let map_accepting_inplace f m =
  Hashtbl.filter_map_inplace (fun k _ -> Some (f k)) m.accepting
;;

let copy m =
  let copytrans = Hashtbl.copy m.transitions in
  Hashtbl.filter_map_inplace (fun _ v -> Some (Hashtbl.copy v)) copytrans;
  { states = m.states
  ; alphabet = m.alphabet
  ; transitions = copytrans
  ; start = m.start
  ; accepting = Hashtbl.copy m.accepting
  }
;;

let create_automata qs alph tran init fin =
  let length = List.length qs in
  let transitions = Hashtbl.create length in
  List.iter
    (fun s ->
      let tbl = Hashtbl.create (List.length alph) in
      Hashtbl.add transitions s tbl)
    qs;
  List.iter (fun (s, a, t) -> Hashtbl.add (Hashtbl.find transitions s) a t) tran;
  let accepting = Hashtbl.create length in
  List.iter (fun s -> Hashtbl.add accepting s (List.mem s fin)) qs;
  { states = qs
  ; alphabet = SS.add_list alph SS.empty
  ; transitions
  ; start = init
  ; accepting
  }
;;