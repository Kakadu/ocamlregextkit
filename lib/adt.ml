module SS = struct
  include Set.Make (Int)

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

(** ASCII code for character on the edge. *)
type mark =
  { mfrom : int
  ; mto : int
  }

let make_mark ~from mto =
  assert (from <= mto);
  { mfrom = from; mto }
;;

let cmp_mark { mfrom; mto } o =
  let c = Int.compare mfrom o.mfrom in
  if c = 0 then Int.compare mto o.mto else c
;;

let mark_contains mark ~other = mark.mfrom <= other.mfrom && other.mto <= mark.mto

(* module Mark_set : sig end = struct
  type elt = mark

  type t =
    | Empty
    | Node of
        { l : t
        ; v : elt
        ; r : t
        ; h : int
        }
end *)

module Mark_map : sig
  type 'a t

  val singleton : mark -> 'a -> 'a t
  val add : mark -> 'a -> 'a t -> 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val cardinal : 'a t -> int
  val length : 'a t -> int
  val find_exn : mark -> 'a t -> 'a
  val find_marks : mark -> 'a t -> mark list
  val find_contained : mark -> 'a t -> 'a list
  val iter : (mark -> 'a -> unit) -> 'a t -> unit
  val fold : (mark -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : (mark -> 'a -> 'b option) -> 'a t -> 'b t
end = struct
  type key = mark

  type 'a t =
    | Empty
    | Node of
        { l : 'a t
        ; v : key
        ; d : 'a
        ; r : 'a t
        ; h : int
        }

  let height = function
    | Empty -> 0
    | Node { h; _ } -> h
  ;;

  let create l x d r =
    let hl = height l
    and hr = height r in
    Node { l; v = x; d; r; h = (if hl >= hr then hl + 1 else hr + 1) }
  ;;

  let singleton x d = Node { l = Empty; v = x; d; r = Empty; h = 1 }

  let bal l x d r =
    let hl =
      match l with
      | Empty -> 0
      | Node { h; _ } -> h
    in
    let hr =
      match r with
      | Empty -> 0
      | Node { h; _ } -> h
    in
    if hl > hr + 2
    then (
      match l with
      | Empty -> invalid_arg "Map.bal"
      | Node { l = ll; v = lv; d = ld; r = lr; _ } ->
        if height ll >= height lr
        then create ll lv ld (create lr x d r)
        else (
          match lr with
          | Empty -> invalid_arg "Map.bal"
          | Node { l = lrl; v = lrv; d = lrd; r = lrr; _ } ->
            create (create ll lv ld lrl) lrv lrd (create lrr x d r)))
    else if hr > hl + 2
    then (
      match r with
      | Empty -> invalid_arg "Map.bal"
      | Node { l = rl; v = rv; d = rd; r = rr; _ } ->
        if height rr >= height rl
        then create (create l x d rl) rv rd rr
        else (
          match rl with
          | Empty -> invalid_arg "Map.bal"
          | Node { l = rll; v = rlv; d = rld; r = rlr; _ } ->
            create (create l x d rll) rlv rld (create rlr rv rd rr)))
    else Node { l; v = x; d; r; h = (if hl >= hr then hl + 1 else hr + 1) }
  ;;

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false
  ;;

  let rec add x data = function
    | Empty -> Node { l = Empty; v = x; d = data; r = Empty; h = 1 }
    | Node { l; v; d; r; h } as m ->
      let c = cmp_mark x v in
      if c = 0
      then if d == data then m else Node { l; v = x; d = data; r; h }
      else if c < 0
      then (
        let ll = add x data l in
        if l == ll then m else bal ll v d r)
      else (
        let rr = add x data r in
        if r == rr then m else bal l v d rr)
  ;;

  let rec find_exn x = function
    | Empty -> raise Not_found
    | Node { l; v; d; r; _ } ->
      let c = cmp_mark x v in
      if c = 0 then d else find_exn x (if c < 0 then l else r)
  ;;

  let find_marks m =
    let rec helper acc = function
      | Empty -> acc
      | Node { l; v; d; r; _ } ->
        let acc = if mark_contains v ~other:m then m :: acc else acc in
        helper (helper acc l) r
    in
    fun map -> helper [] map
  ;;

  let find_contained m =
    let rec helper acc = function
      | Empty -> acc
      | Node { l; v; d; r; _ } ->
        let acc = if mark_contains v ~other:m then d :: acc else acc in
        helper (helper acc l) r
    in
    fun map -> helper [] map
  ;;

  let rec cardinal = function
    | Empty -> 0
    | Node { l; r; _ } -> cardinal l + 1 + cardinal r
  ;;

  let length = cardinal

  let rec iter f = function
    | Empty -> ()
    | Node { l; v; d; r; _ } ->
      iter f l;
      f v d;
      iter f r
  ;;

  let rec fold f m accu =
    match m with
    | Empty -> accu
    | Node { l; v; d; r; _ } -> fold f r (f v d (fold f l accu))
  ;;

  let rec map f = function
    | Empty -> Empty
    | Node { l; v; d; r; h } ->
      let l' = map f l in
      let d' = f d in
      let r' = map f r in
      Node { l = l'; v; d = d'; r = r'; h }
  ;;

  let rec min_binding = function
    | Empty -> raise Not_found
    | Node { l = Empty; v; d; _ } -> v, d
    | Node { l; _ } -> min_binding l
  ;;

  let rec max_binding = function
    | Empty -> raise Not_found
    | Node { v; d; r = Empty; _ } -> v, d
    | Node { r; _ } -> max_binding r
  ;;

  let rec add_min_binding k x = function
    | Empty -> singleton k x
    | Node { l; v; d; r; _ } -> bal (add_min_binding k x l) v d r
  ;;

  let rec add_max_binding k x = function
    | Empty -> singleton k x
    | Node { l; v; d; r; _ } -> bal l v d (add_max_binding k x r)
  ;;

  let rec join l v d r =
    match l, r with
    | Empty, _ -> add_min_binding v d r
    | _, Empty -> add_max_binding v d l
    | ( Node { l = ll; v = lv; d = ld; r = lr; h = lh }
      , Node { l = rl; v = rv; d = rd; r = rr; h = rh } ) ->
      if lh > rh + 2
      then bal ll lv ld (join lr v d r)
      else if rh > lh + 2
      then bal (join l v d rl) rv rd rr
      else create l v d r
  ;;

  let rec remove_min_binding = function
    | Empty -> invalid_arg "Map.remove_min_elt"
    | Node { l = Empty; r; _ } -> r
    | Node { l; v; d; r; _ } -> bal (remove_min_binding l) v d r
  ;;

  let concat t1 t2 =
    match t1, t2 with
    | Empty, t -> t
    | t, Empty -> t
    | _, _ ->
      let x, d = min_binding t2 in
      join t1 x d (remove_min_binding t2)
  ;;

  let rec filter_map f = function
    | Empty -> Empty
    | Node { l; v; d; r; _ } ->
      (* call [f] in the expected left-to-right order *)
      let l' = filter_map f l in
      let fvd = f v d in
      let r' = filter_map f r in
      (match fvd with
       | Some d' -> join l' v d' r'
       | None -> concat l' r')
  ;;
end

[@@@ocaml.warnerror "-32"]

(* Label for empty transitions *)
let eps : mark = make_mark ~from:min_int min_int

let halfmark_of_string = function
  | "ε" -> min_int
  | s when String.length s = 1 -> Char.code s.[0]
  | _ -> assert false
;;

let mark_of_string_exn s =
  let half = halfmark_of_string s in
  make_mark ~from:half half
;;

let mark_of_char chr =
  let c = Char.code chr in
  make_mark ~from:c c
;;

let string_of_mark m =
  if m = eps
  then "ε"
  else if m.mto < 128
  then Printf.sprintf "%d..%d" m.mfrom m.mto
  else failwith "Unicode not supported"
;;

let intersect_mark m1 m2 = if cmp_mark m1 m2 = 0 then Some m1 else None
let inject_transitions tran = List.map (fun (a, b, c) -> a, mark_of_string_exn b, c) tran

type 't automata =
  { mutable states : 't list
  ; mutable alphabet : SS.t
  ; transitions : ('t, 't Mark_map.t) Hashtbl.t
  ; mutable start : 't
  ; accepting : ('t, bool) Hashtbl.t
  }

let map f from =
  let states = List.map f from.states in
  let start = f from.start in
  let transitions : (_, _ Mark_map.t) Hashtbl.t =
    Hashtbl.create (Hashtbl.length from.transitions)
  in
  Hashtbl.iter
    (fun k v ->
      let newk = f k in
      (* let newv = Hashtbl.create (Mark_map.cardinal v) in *)
      (* let newv = Mark_map.empty in *)
      (* let newv =
         Mark_map.fold (fun k v acc -> Mark_map.add k (f v) acc) v Mark_map.empty
         in *)
      let newv = Mark_map.map f v in
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
    (fun s ats acc -> Mark_map.fold (fun a t acc' -> (s, a, t) :: acc') ats acc)
    m.transitions
    []
;;

let iter_transitions f m =
  Hashtbl.iter (fun s v -> Mark_map.iter (fun a t -> f (s, a, t)) v) m.transitions
;;

let map_transitions f m =
  Hashtbl.fold
    (fun s v acc -> Mark_map.fold (fun a t acc' -> f (s, a, t) :: acc') v acc)
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

let get_next_states m s (a : mark) =
  Mark_map.find_contained a (Hashtbl.find m.transitions s)
;;

let get_prev_states : 't automata -> 't -> mark -> 't list =
  fun m t a ->
  Hashtbl.fold
    (fun s v acc ->
      Mark_map.fold (fun a' t' acc' -> if a = a' && t = t' then s :: acc' else acc') v acc)
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
  let marks_of_alphabet =
    (* TODO: fix hack that hardcodes epsilon as min_int *)
    SS.add min_int m.alphabet
  in
  let rec find_reachable_states : _ list -> _ list =
    fun marked ->
    let newmarked =
      List.fold_left
        (fun acc s ->
          SS.fold_left
            (fun acc2 a ->
              Utils.list_union acc2 (get_next_states m s (make_mark ~from:a a)))
            acc
            marks_of_alphabet)
        marked
        marked
    in
    if marked <> newmarked then find_reachable_states newmarked else newmarked
  in
  find_reachable_states [ m.start ]
;;

let filter_states_inplace m f =
  set_states m (List.filter f m.states);
  Hashtbl.filter_map_inplace (fun s ts -> if f s then Some ts else None) m.transitions;
  Hashtbl.filter_map_inplace (fun s b -> if f s then Some b else None) m.accepting
;;

let merge_states_inplace m p q =
  filter_states_inplace m (fun s -> s <> q);
  Hashtbl.filter_map_inplace
    (fun _ v ->
      Option.some @@ Mark_map.filter_map (fun _ t -> Some (if t = q then p else t)) v)
    m.transitions;
  if m.start = q then m.start <- p
;;

let add_to_alphabet m alph = set_alphabet m (SS.union m.alphabet alph)

let map_accepting_inplace f m =
  Hashtbl.filter_map_inplace (fun k _ -> Some (f k)) m.accepting
;;

let copy m =
  let copytrans = Hashtbl.copy m.transitions in
  Hashtbl.filter_map_inplace (fun _ v -> Some v) copytrans;
  { states = m.states
  ; alphabet = m.alphabet
  ; transitions = copytrans
  ; start = m.start
  ; accepting = Hashtbl.copy m.accepting
  }
;;

let create_automata qs (alph : string list) tran init fin =
  let length = List.length qs in
  let transitions = Hashtbl.create length in
  List.iter
    (fun s ->
      let tbl = Hashtbl.create (List.length alph) in
      Hashtbl.add transitions s tbl)
    qs;
  List.iter
    (fun (s, (a : string), t) ->
      Hashtbl.add (Hashtbl.find transitions s) (mark_of_string_exn a) t)
    tran;
  let accepting = Hashtbl.create length in
  List.iter (fun s -> Hashtbl.add accepting s (List.mem s fin)) qs;
  { states = qs
  ; alphabet = SS.add_list (List.map halfmark_of_string alph) SS.empty
  ; transitions
  ; start = init
  ; accepting
  }
;;

let create_automata_gen qs alphabet tran init fin =
  let length = List.length qs in
  let transitions = Hashtbl.create length in
  List.iter
    (fun s ->
      let tbl = Hashtbl.create (SS.cardinal alphabet) in
      Hashtbl.add transitions s tbl)
    qs;
  List.iter (fun (s, a, t) -> Hashtbl.add (Hashtbl.find transitions s) a t) tran;
  let accepting = Hashtbl.create length in
  List.iter (fun s -> Hashtbl.add accepting s (List.mem s fin)) qs;
  { states = qs; alphabet; transitions; start = init; accepting }
;;
