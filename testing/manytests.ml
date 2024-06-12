open Regextkit

let lang_of_regex s =
  match Re.parse s with
  | exception Re.Syntax_error _ -> assert false
  | r -> Nfa.re_to_nfa r
;;

let%test _ =
  let nfa1 = lang_of_regex "a*" in
  let nfa2 = lang_of_regex "b*" in
  let i = Nfa.intersect nfa1 nfa2 in
  not (Nfa.is_empty i)
;;

let%expect_test _ =
  let ast = Re.parse "b*" in
  let nfa = Nfa.re_to_nfa ast in
  Nfa.prune nfa;
  let dfa = Dfa.nfa_to_dfa nfa in
  Printf.printf "%b" (Dfa.is_empty dfa);
  [%expect {|false|}]
;;

let%expect_test "intersect b* and bb*" =
  let nfa1 = Re.parse "b*" |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let nfa2 = Re.parse "bb*" |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  let nfa3 = Nfa.intersect nfa1 nfa2 in
  Nfa.prune nfa3;
  Printf.printf "%b" (Nfa.is_empty nfa3);
  (* BUG *)
  [%expect {| true |}]
;;

let%expect_test "intersect DFA (ab)* and c(ab)*" =
  let nfa1 = Re.parse "(ab)*" |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let nfa2 = Re.parse "c(ab)*" |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  Nfa.merge_alphabets nfa1 nfa2;
  let dfa1 = Dfa.nfa_to_dfa nfa1 in
  let dfa2 = Dfa.nfa_to_dfa nfa2 in
  let dfa3 = Dfa.product_intersection dfa1 dfa2 in
  Printf.printf "%b" (Dfa.is_empty dfa3);
  [%expect {| true |}]
;;

let%expect_test "intersect NFA a* and ca*" =
  let nfa1 = Re.parse "a*" |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let nfa2 = Re.parse "ca*" |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  Nfa.merge_alphabets nfa1 nfa2;
  let nfa3 = Nfa.intersect nfa1 nfa2 in
  Nfa.prune nfa3;
  Printf.printf "%b" (Nfa.is_empty nfa3);
  [%expect {| true |}]
;;

let%expect_test _ =
  let lang1 = "(ka)*du((screams)+(sings))(ka)*du*" in
  let lang2 = "kakadu((says)+(sings)+(screams))(loudly)?kaka(ka)*duu*" in
  let nfa1 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let nfa2 = Re.parse lang2 |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  Nfa.merge_alphabets nfa1 nfa2;
  let nfa3 = Nfa.intersect nfa1 nfa2 in
  Nfa.prune nfa3;
  Printf.printf "%b" (Nfa.is_empty nfa3);
  (* BUG *)
  [%expect {| true |}]
;;
