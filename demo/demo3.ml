open Regextkit

let make_alphabet lst =
  let open Regextkit.Tree in
  let f acc x = Union (acc, Literal x) in
  let allowed_chars = List.concat lst in
  List.fold_left f (Literal ".") allowed_chars
;;

let java_alphabet =
  let wrap start i = String.make 1 @@ Char.chr (Char.code start + i) in
  make_alphabet
    [ List.init 26 (wrap 'a'); List.init 26 (wrap 'A'); List.init 10 (wrap '0') ]
;;

let describe_nfa nfa =
  Format.printf "states: %d\n%!" (List.length  (Nfa.get_states nfa))

let () =
  let nfa1 = Re.parse "ThisValueWillMatch" |> Regextkit.Nfa.re_to_nfa in
  Regextkit.Nfa.prune nfa1;
  print_endline "NFA1";
  describe_nfa nfa1;
  let ast2 = Re.parse "WillMatch" in
  let ast2 = Regextkit.Tree.(Concat (Star java_alphabet, ast2)) in
  let nfa2 = ast2 |> Regextkit.Nfa.re_to_nfa in
  Regextkit.Nfa.prune nfa2;
  print_endline "NFA2";
  describe_nfa nfa2;
  let nfa3 = Nfa.intersect nfa1 nfa2 in
  print_endline "NFA3";
  describe_nfa nfa3;
  Printf.printf "is_empty: %b\n" (Nfa.is_empty nfa3);
  ()
;;
