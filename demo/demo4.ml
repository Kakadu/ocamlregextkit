open Regextkit

let describe_dfa nfa = Format.printf "states: %d\n%!" (List.length (Dfa.get_states nfa))

let () =
  let dfa1 =
    Re.parse "kakadu1((says)+(sings)+(screams))(1loudly+ε)1kaka(ka)*duu*"
    |> Regextkit.Dfa.re_to_dfa
  in
  print_endline "DFA1";
  describe_dfa dfa1;
  let ast2 = Re.parse "(ka)*du1((screams)+(sings))1(ka)*du*" in
  let dfa2 = ast2 |> Regextkit.Dfa.re_to_dfa in
  print_endline "DFA2";
  describe_dfa dfa2;
  Nfa.merge_alphabets dfa1 dfa2;
  let dfa3 = Dfa.product_intersection dfa1 dfa2 in
  print_endline "DFA3";
  describe_dfa dfa3;
  let () = Dfa.minimise dfa3 in
  print_endline "DFA3 minimized";
  describe_dfa dfa3;
  Printf.printf "is_empty: %b\n" (Dfa.is_empty dfa3);
  ()
;;
