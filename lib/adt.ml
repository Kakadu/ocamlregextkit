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

type 't automata =
  | A :
      { mutable states : ('t, 'cmp) Base.Set.t
      ; mutable alphabet : SS.t
      ; transitions : ('t, (string, 't) Hashtbl.t) Hashtbl.t
      ; mutable start : 't
      ; accepting : ('t, bool) Hashtbl.t
      }
      -> 't automata

(* type 't automata =
  { mutable states : 't list
  ; mutable alphabet : SS.t
  ; transitions : ('t, (string, 't) Hashtbl.t) Hashtbl.t
  ; mutable start : 't
  ; accepting : ('t, bool) Hashtbl.t
  } *)

let map (type b) new_compare f (A from) =
  let module CmpB : Base.Comparator.S with type t = b = struct
    type t = b

    include Base.Comparator.Make (struct
        type t = b

        let compare = new_compare
        let sexp_of_t _ = assert false
      end)
  end
  in
  let states = Base.Set.map (module CmpB) from.states ~f in
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
  A { states; start; transitions; alphabet = from.alphabet; accepting }
;;

(* let get_states (A m) = m.states
   let set_states m qs = m.states <- qs *)
let iter_states f (A m) = Base.Set.iter ~f m.states

(* let filter_states f (A m) = Base.Set.filter ~f m.states *)
let exists_states f (A m) = Base.Set.exists ~f m.states
let for_all_states f (A m) = Base.Set.for_all ~f m.states

(* let map_states f (A m) = Base.Set.map ~f m.states *)
let get_alphabet (A m) = m.alphabet
let set_alphabet (A m) alph = m.alphabet <- alph
let iter_alphabet f (A m) = SS.iter f m.alphabet

(* let filter_alphabet f m = SS.filter f m.alphabet *)
(* let map_alphabet f m = SS.map f m.alphabet *)
let exists_alphabet f (A m) = SS.exists f m.alphabet
let for_all_alphabet f (A m) = SS.for_all f m.alphabet

let get_transitions (A m) =
  Hashtbl.fold
    (fun s ats acc -> Hashtbl.fold (fun a t acc' -> (s, a, t) :: acc') ats acc)
    m.transitions
    []
;;

let iter_transitions f (A m) =
  Hashtbl.iter (fun s v -> Hashtbl.iter (fun a t -> f (s, a, t)) v) m.transitions
;;

let map_transitions f (A m) =
  Hashtbl.fold
    (fun s v acc -> Hashtbl.fold (fun a t acc' -> f (s, a, t) :: acc') v acc)
    m.transitions
    []
;;

let get_start (A m) = m.start

let get_accepting (A m) =
  Hashtbl.fold (fun k v acc -> if v then k :: acc else acc) m.accepting []
;;

let iter_accepting f (A m) = Hashtbl.iter (fun s v -> if v then f s) m.accepting

let map_accepting f (A m) =
  Hashtbl.fold (fun s v acc -> if v then f s :: acc else acc) m.accepting []
;;

let get_next_states (A m) s a = Hashtbl.find_all (Hashtbl.find m.transitions s) a

let get_prev_states (A m) t a =
  Hashtbl.fold
    (fun s v acc ->
      Hashtbl.fold (fun a' t' acc' -> if a = a' && t = t' then s :: acc' else acc') v acc)
    m.transitions
    []
;;

let is_accepting (A m) s =
  match Hashtbl.find_opt m.accepting s with
  | None -> false
  | Some res -> res
;;

let get_reachable_states : 'a. 'a automata -> 'a list =
  fun (A m as adt) ->
  let rec find_reachable_states : _ list -> _ list =
    fun marked ->
    let newmarked =
      List.fold_left
        (fun acc s ->
          SS.fold_left
            (fun acc2 a -> Utils.list_union acc2 (get_next_states adt s a))
            acc
            (SS.add "ε" m.alphabet))
        marked
        marked
    in
    if marked <> newmarked then find_reachable_states newmarked else newmarked
  in
  find_reachable_states [ m.start ]
;;

let filter_states_inplace (A m) f =
  m.states <- Base.Set.filter m.states ~f;
  (* set_states m (List.filter f m.states); *)
  Hashtbl.filter_map_inplace (fun s ts -> if f s then Some ts else None) m.transitions;
  Hashtbl.filter_map_inplace (fun s b -> if f s then Some b else None) m.accepting
;;

let merge_states_inplace (A m as adt) p q =
  filter_states_inplace adt (fun s -> s <> q);
  Hashtbl.iter
    (fun _ v -> Hashtbl.filter_map_inplace (fun _ t -> Some (if t = q then p else t)) v)
    m.transitions;
  if m.start = q then m.start <- p
;;

let add_to_alphabet (A m as adt) alph = set_alphabet adt (SS.union m.alphabet alph)

let map_accepting_inplace f (A m) =
  Hashtbl.filter_map_inplace (fun k _ -> Some (f k)) m.accepting
;;

let copy (A m) =
  let copytrans = Hashtbl.copy m.transitions in
  Hashtbl.filter_map_inplace (fun _ v -> Some (Hashtbl.copy v)) copytrans;
  A
    { states = m.states
    ; alphabet = m.alphabet
    ; transitions = copytrans
    ; start = m.start
    ; accepting = Hashtbl.copy m.accepting
    }
;;

let create_automata (type a) state_comparator qs alph tran init fin =
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
  let module Cmp : Base.Comparator.S with type t = a = struct
    type t = a

    include Base.Comparator.Make (struct
        type t = a

        let compare = state_comparator
        let sexp_of_t _ = assert false
      end)
  end
  in
  A
    { states = Base.Set.of_list (module Cmp) qs
    ; alphabet = SS.add_list alph SS.empty
    ; transitions
    ; start = init
    ; accepting
    }
;;
