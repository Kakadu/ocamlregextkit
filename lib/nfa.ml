open Tree

type state = int
type nfa = state Adt.automata
(* TODO(Kakadu): OCaml convention recommends to call a type realted to module by a name 't' *)

let get_states = Adt.get_states
let get_alphabet = Adt.get_alphabet
let get_transitions = Adt.get_transitions
let get_start = Adt.get_start
let get_accepting = Adt.get_accepting

(* |is_accepting| -- returns true if state s is accepting *)
let is_accepting = Adt.is_accepting

(* |print| -- prints out nfa representation *)
let print n =
  print_string "states: ";
  Adt.iter_states
    (fun s ->
      print_int s;
      print_char ' ')
    n;
  print_newline ();
  print_string "alphabet: ";
  Adt.iter_alphabet
    (fun a ->
      print_string a;
      print_char ' ')
    n;
  print_newline ();
  print_string "start: ";
  print_int (get_start n);
  print_newline ();
  print_string "accepting: ";
  Adt.iter_accepting
    (fun s ->
      print_int s;
      print_char ' ')
    n;
  print_newline ();
  print_string "transitions: ";
  print_newline ();
  Adt.iter_transitions
    (fun (s, a, t) ->
      print_string "    ";
      print_int s;
      print_string ("\t--" ^ a ^ "-->\t");
      print_int t;
      print_newline ())
    n
;;

(* |export_graphviz| -- exports the nfa in the DOT language for Graphviz *)
let export_graphviz n =
  Printf.sprintf
    "digraph G {\n n0 [label=\"\", shape=none, height=0, width=0, ]\n%s\nn0 -> %s;\n%s\n}"
    (List.fold_left
       (fun a s ->
         let shape = "ellipse, " ^ if is_accepting n s then "peripheries=2, " else "" in
         Printf.sprintf "%s\"%s\" [shape=%s];\n" a (string_of_int s) shape)
       ""
       (get_states n))
    (string_of_int (get_start n))
    (List.fold_left
       (fun acc (s, a, t) ->
         Printf.sprintf
           "%s%s -> %s [label=\"%s\", ];\n"
           acc
           (string_of_int s)
           (string_of_int t)
           a)
       ""
       (get_transitions n))
;;

(* |eps_reachable_set| -- returns set of all epsilon-reachable states from input set of states *)
let eps_reachable_set n ss =
  let get_reachable_set states =
    List.fold_right
      (fun s acc -> Utils.list_union acc (Adt.get_next_states n s "ε"))
      states
      states
  in
  (* iterate reachable set until no changes *)
  let sts = ref (get_reachable_set ss) in
  let newSts = ref (get_reachable_set !sts) in
  while !sts <> !newSts do
    sts := !newSts;
    newSts := get_reachable_set !sts
  done;
  List.sort compare !sts
;;

(* |reachable_states| -- returns the set of reachable states in nfa n *)
let reachable_states = Adt.get_reachable_states

(* |succ| -- the resulting states of nfa n after reading symbol *)
let succ n state symbol =
  let initial_reachable = eps_reachable_set n [ state ] in
  let succs =
    List.fold_right
      Utils.add_unique
      (List.concat_map (fun s -> Adt.get_next_states n s symbol) initial_reachable)
      []
  in
  eps_reachable_set n succs
;;

(* |pred| -- returns the set of states preceeding state in nfa n *)
let pred n state =
  (* all states from which state is eps_reachable *)
  let epspreds =
    Adt.filter_states (fun s -> List.mem state (eps_reachable_set n [ s ])) n
  in
  let preds =
    List.fold_left
      (fun acc s ->
        List.fold_left
          (fun acc' a -> Utils.list_union acc' (Adt.get_prev_states n s a))
          acc
          (get_alphabet n))
      []
      epspreds
  in
  Adt.filter_states
    (fun s -> List.exists (fun ss -> List.mem ss preds) (eps_reachable_set n [ s ]))
    n
;;

(* |prune| -- reduces input nfa by pruning unreachable states *)
let prune n =
  let marked = reachable_states n in
  Adt.filter_states_inplace n (fun s -> List.mem s marked)
;;

(* |is_empty| -- returns true iff nfa has no reachable accepting states *)
let is_empty n =
  let marked = reachable_states n in
  not (List.exists (is_accepting n) marked)
;;

(* |is_accepted| -- returns true iff string s is accepted by the nfa n. Can take a long time *)
let is_accepted n s =
  let sts = ref (eps_reachable_set n [ get_start n ]) in
  for i = 0 to String.length s - 1 do
    let c = String.make 1 s.[i] in
    sts
      := eps_reachable_set
           n
           (List.fold_left (fun a ss -> Utils.list_union (succ n ss c) a) [] !sts)
  done;
  List.exists (is_accepting n) !sts
;;

(* |get_accepted| -- returns the shortest word accepted by dfa m *)
let get_accepted n =
  let queue = ref (List.rev_map (fun s -> s, "") (eps_reachable_set n [ get_start n ]))
  and seen = ref []
  and shortest = ref None in
  while Option.is_none !shortest && List.length !queue > 0 do
    let currentState, currentWord = List.hd !queue in
    if is_accepting n currentState
    then shortest := Some currentWord
    else (
      seen := currentState :: !seen;
      let newteps =
        List.filter_map
          (fun t -> if not (List.mem t !seen) then Some (t, currentWord) else None)
          (Adt.get_next_states n currentState "ε")
      and newt =
        List.concat_map
          (fun a ->
            let nexts = Adt.get_next_states n currentState a in
            List.filter_map
              (fun t ->
                if not (List.mem t !seen) then Some (t, currentWord ^ a) else None)
              nexts)
          (get_alphabet n)
      in
      queue := List.tl !queue @ newteps @ newt)
  done;
  !shortest
;;

(* |merge_alphabets| -- returns pair of nfas with a common alphabet *)
let merge_alphabets n1 n2 =
  let alphabet1 = get_alphabet n1
  and alphabet2 = get_alphabet n2 in
  Adt.add_to_alphabet n1 alphabet2;
  Adt.add_to_alphabet n2 alphabet1
;;

(* |copy| -- Creates a deep copy of NFA *)
let copy = Adt.copy

(* |create| -- Creates NFA, Renames states as their index in qs *)
let create qs alph tran init fin =
  (* Check parameters for correctness *)
  if not (List.mem init qs)
  then raise (Invalid_argument "NFA Initial State not in States");
  List.iter
    (fun f ->
      if not (List.mem f qs)
      then raise (Invalid_argument "NFA Accepting State not in States"))
    fin;
  List.iter
    (fun (s, a, t) ->
      if not ((a = "ε" || List.mem a alph) && List.mem s qs && List.mem t qs)
      then raise (Invalid_argument "NFA Transition not valid"))
    tran;
  let newstates = List.init (List.length qs) Fun.id in
  let newinit = Option.get (Utils.index init qs)
  and newtran =
    List.rev_map
      (fun (s, a, t) -> Option.get (Utils.index s qs), a, Option.get (Utils.index t qs))
      tran
  and newfin = List.rev_map (fun s -> Option.get (Utils.index s qs)) fin in
  Adt.create_automata newstates alph newtran newinit newfin
;;

let counter = ref 0

(* |construct_rec_nfa| -- recursively builds NFA for given re *)
let rec construct_rec_nfa = function
  | Literal a ->
    incr counter;
    incr counter;
    let states = [ !counter - 2; !counter - 1 ]
    and alphabet = [ a ]
    and transitions = [ !counter - 2, a, !counter - 1 ]
    and start = !counter - 2
    and accepting = [ !counter - 1 ] in
    states, alphabet, transitions, start, accepting
  | Epsilon ->
    incr counter;
    let states = [ !counter - 1 ]
    and alphabet = []
    and transitions = []
    and start = !counter - 1
    and accepting = [ !counter - 1 ] in
    states, alphabet, transitions, start, accepting
  | Empty ->
    incr counter;
    let states = [ !counter - 1 ]
    and alphabet = []
    and transitions = []
    and start = !counter - 1
    and accepting = [] in
    states, alphabet, transitions, start, accepting
  | Union (r1, r2) ->
    let s1, a1, t1, i1, f1 = construct_rec_nfa r1
    and s2, a2, t2, i2, f2 = construct_rec_nfa r2 in
    incr counter;
    let states = (!counter - 1) :: (s1 @ s2)
    and alphabet = Utils.list_union a1 a2
    and transitions = ((!counter - 1, "ε", i1) :: t1) @ ((!counter - 1, "ε", i2) :: t2)
    and start = !counter - 1
    and accepting = f1 @ f2 in
    states, alphabet, transitions, start, accepting
  | Concat (r1, r2) ->
    let s1, a1, t1, i1, f1 = construct_rec_nfa r1
    and s2, a2, t2, i2, f2 = construct_rec_nfa r2 in
    let newtrans = List.rev_map (fun s -> s, "ε", i2) f1 in
    let states = s1 @ s2
    and alphabet = Utils.list_union a1 a2
    and transitions = t1 @ newtrans @ t2
    and start = i1
    and accepting = f2 in
    states, alphabet, transitions, start, accepting
  | Star r ->
    let s1, a1, t1, i1, f1 = construct_rec_nfa r in
    incr counter;
    let newtrans = List.rev_map (fun s -> s, "ε", i1) f1 in
    let states = (!counter - 1) :: s1
    and alphabet = a1
    and transitions = ((!counter - 1, "ε", i1) :: newtrans) @ t1
    and start = !counter - 1
    and accepting = (!counter - 1) :: f1 in
    states, alphabet, transitions, start, accepting
;;

(* |re_to_nfa| -- converts input regex AST into nfa *)
let re_to_nfa re =
  counter := 0;
  let states, alphabet, transitions, start, accepting = construct_rec_nfa re in
  Adt.create_automata
    (List.sort compare states)
    (List.rev alphabet)
    transitions
    start
    accepting
;;

let intersect left right =
  (* TODO: merge alphabets *)
  let unionAlphabet = Utils.list_union (get_alphabet left) (get_alphabet right) in
  let last_state = ref 0 in
  let new_states : (state * state, state) Hashtbl.t = Hashtbl.create 42 in
  let cartAccepting = ref [] in
  let cartTrans = ref [] in
  let () =
    Adt.iter_states
      (fun s1 ->
        Adt.iter_states
          (fun s2 ->
            incr last_state;
            Hashtbl.add new_states (s1, s2) !last_state;
            if Adt.is_accepting left s1 && Adt.is_accepting right s2
            then cartAccepting := !last_state :: !cartAccepting)
          right)
      left
  in
  left
  |> Adt.iter_transitions (fun (slf, cl, slt) ->
    right
    |> Adt.iter_transitions (fun (srf, cr, srt) ->
      if cl = cr
      then (
        match Hashtbl.find new_states (slf, srf), Hashtbl.find new_states (slt, srt) with
        | exception Not_found ->
          (* It could happen after minimization *)
          ()
        | new_from, new_to -> cartTrans := (new_from, cl, new_to) :: !cartTrans)));
  Adt.create_automata
    (Hashtbl.to_seq_values new_states |> List.of_seq)
    unionAlphabet
    !cartTrans
    (Hashtbl.find new_states (Adt.get_start left, Adt.get_start right))
    !cartAccepting
;;
