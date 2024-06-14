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
  (* let nfa1 =
     Nfa.create [ 0; 1; 2 ] [ "b" ] [ 2, "ε", 0; 0, "b", 1; 1, "ε", 0 ] 2 [ 1; 2 ]
     in *)
  Printf.printf "%b\n" (Nfa.is_empty nfa1);
  (* print_endline (Nfa.export_graphviz nfa1); *)
  [%expect {|
    false |}];
  let nfa2 = Re.parse "bb*" |> Nfa.re_to_nfa in
  (* let nfa2 = Tree.(Concat (Literal "b", Star (Literal "b"))) |> Nfa.re_to_nfa in
     Nfa.prune nfa2; *)
  (* let nfa2 =
     Nfa.create
     [ 0; 1; 2; 3; 4 ]
     [ "b" ]
     [ 0, "b", 1; 1, "ε", 4; 4, "ε", 2; 3, "ε", 2; 2, "b", 3 ]
     0
     [ 4; 3 ]
     in *)
  Printf.printf "%b\n" (Nfa.is_empty nfa2);
  (* print_endline (Nfa.export_graphviz nfa2); *)
  [%expect {|
    false |}];
  (* let nfa1 = Nfa.create [ 1 ] [ "b" ] [ 1, "ε", 1; 1, "b", 1 ] 1 [ 1 ] in
     Printf.printf "%b\n" (Nfa.is_empty nfa1);
     let nfa2 = Nfa.create [ 0; 1 ] [ "b" ] [ 0, "b", 1; 1, "ε", 1; 1, "b", 1 ] 0 [ 1 ] in
     Printf.printf "%b\n" (Nfa.is_empty nfa2); *)
  let nfa3 = Nfa.intersect ~verbose:false nfa1 nfa2 in
  [%expect {| |}];
  Nfa.prune nfa3;
  (* print_endline (Nfa.export_graphviz nfa3); *)
  Printf.printf "empty intersection: %b\n" (Nfa.is_empty nfa3);
  [%expect {|
    empty intersection: false |}];
  assert (Nfa.is_accepted nfa3 "b");
  [%expect {||}];
  Printf.printf "accepting '': %b\n" (Nfa.is_accepted nfa3 "");
  Printf.printf "accepting 'b': %b\n" (Nfa.is_accepted nfa3 "b");
  Printf.printf "accepting 'bb': %b\n" (Nfa.is_accepted nfa3 "bb");
  [%expect {|
    accepting '': false
    accepting 'b': true
    accepting 'bb': true |}]
;;

let%expect_test "Equivalent language. Distinct automata" =
  let nfa1 = Nfa.create [ 1; 2; 3 ] [ "b" ] [ 1, "ε", 2; 2, "b", 3 ] 1 [ 3 ] in
  Printf.printf "%b\n" (Nfa.is_empty nfa1);
  let nfa2 = Nfa.create [ 4; 5 ] [ "b" ] [ 4, "b", 5 ] 4 [ 5 ] in
  Printf.printf "%b\n" (Nfa.is_empty nfa2);
  let nfa3 = Nfa.intersect ~verbose:false nfa1 nfa2 in
  (* Nfa.prune nfa3; *)
  Printf.printf "empty intersection: %b\n" (Nfa.is_empty nfa3);
  Printf.printf "accepting 'b': %b\n" (Nfa.is_accepted nfa3 "b");
  Printf.printf "accepting 'bb': %b\n" (Nfa.is_accepted nfa3 "bb");
  (* print_endline (Nfa.export_graphviz nfa3); *)
  [%expect
    {|
    false
    false
    empty intersection: false
    accepting 'b': true
    accepting 'bb': false
     |}]
;;

let%expect_test "intersect NFA a* and ca*" =
  let nfa1 = Re.parse "a*" |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let nfa2 = Re.parse "ca*" |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  Nfa.merge_alphabets nfa1 nfa2;
  let nfa3 = Nfa.intersect nfa1 nfa2 in
  Nfa.prune nfa3;
  Printf.printf "Checking via NFA: %b\n" (Nfa.is_empty nfa3);
  [%expect {|
    Checking via NFA: true |}]
;;

let%expect_test "intersect (ab)* and c(ab)*" =
  let nfa1 = Re.parse "(ab)*" |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let nfa2 = Re.parse "c(ab)*" |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  Nfa.merge_alphabets nfa1 nfa2;
  let dfa1 = Dfa.nfa_to_dfa nfa1 in
  let dfa2 = Dfa.nfa_to_dfa nfa2 in
  let dfa3 = Dfa.product_intersection dfa1 dfa2 in
  Printf.printf "Checking via DFA: %b\n" (Dfa.is_empty dfa3);
  [%expect {| Checking via DFA: true |}];
  let nfa4 = Nfa.intersect nfa1 nfa2 in
  Printf.printf "Checking via NFA: %b\n" (Nfa.is_empty nfa4);
  [%expect {| Checking via NFA: true |}]
;;

let%expect_test "optional is not supported in syntax" =
  let lang1 = "b" in
  let lang2 = "(ε+b)" in
  let nfa1 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let nfa2 = Re.parse lang2 |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  Nfa.merge_alphabets nfa1 nfa2;
  let nfa3 = Nfa.intersect nfa1 nfa2 in
  Nfa.prune nfa3;
  Printf.printf "%b" (Nfa.is_empty nfa3);
  (* BUG *)
  [%expect {| false |}]
;;

let%expect_test "NFA intersection" =
  let lang1 = "(ka)*du((screams)+(sings))(ka)*du*" in
  let lang2 = "kakadu((says)+(sings)+(screams))(loudly+ε)kaka(ka)*duu*" in
  let nfa1 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let nfa2 = Re.parse lang2 |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  Nfa.merge_alphabets nfa1 nfa2;
  let nfa3 = Nfa.intersect nfa1 nfa2 in
  Nfa.prune nfa3;
  Printf.printf "%b" (Nfa.is_empty nfa3);
  (* BUG *)
  [%expect {|
    false |}]
;;

let%expect_test "NFA of DFA" =
  let lang1 = "b*" in
  let nfa1 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  Printf.printf "%b " (Nfa.is_empty nfa1);
  let dfa2 = Dfa.nfa_to_dfa nfa1 in
  let nfa3 = Dfa.nfa_of_dfa dfa2 in
  Printf.printf "%b " (Nfa.is_empty nfa3);
  [%expect {| false false |}]
;;

let%expect_test "Intesection with self-complement via DFA" =
  let lang1 = "b*" in
  let nfa1 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let dfa1 = Dfa.nfa_to_dfa nfa1 in
  let nfa2 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  let dfa2 = Dfa.nfa_to_dfa nfa2 in
  let dfa2 = Dfa.complement dfa2 in
  let dfa3 = Dfa.product_intersection dfa1 dfa2 in
  Printf.printf "%b" (Dfa.is_empty dfa3);
  [%expect {| true |}]
;;

let%expect_test "Intersection with self-complement via NFA" =
  let lang1 = "b*" in
  let nfa1 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let dfa2 = Dfa.nfa_to_dfa nfa1 in
  let dfa3 = Dfa.complement dfa2 in
  let nfa4 = Dfa.nfa_of_dfa dfa3 in
  let nfa5 = Nfa.intersect nfa4 nfa1 in
  Printf.printf "%b" (Nfa.is_empty nfa5);
  [%expect {| true |}]
;;

let%expect_test "Intersection with complement is not always empty (NFA)" =
  let lang1 = "(aa+aaa)" in
  let nfa1 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  let lang1 = "aaaa*" in
  let nfa2 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa2;
  let dfa3 = Dfa.nfa_to_dfa nfa2 in
  let dfa4 = Dfa.complement dfa3 in
  let nfa5 = Dfa.nfa_of_dfa dfa4 in
  let nfa5 = Nfa.intersect nfa1 nfa5 in
  Printf.printf "%b\n" (Nfa.is_empty nfa5);
  Printf.printf "accepting '': %b\n" (Nfa.is_accepted nfa5 "");
  Printf.printf "accepting 'a': %b\n" (Nfa.is_accepted nfa5 "a");
  Printf.printf "accepting 'aa': %b\n" (Nfa.is_accepted nfa5 "aa");
  Printf.printf "accepting 'aaa': %b\n" (Nfa.is_accepted nfa5 "aaa");
  [%expect
    {|
    false
    accepting '': false
    accepting 'a': false
    accepting 'aa': true
    accepting 'aaa': false |}]
;;

let has_word nfa s = Printf.printf "accepting '%s': %b\n" s (Nfa.is_accepted nfa s)

let%expect_test " " =
  let lang2 = "b(a+b)*" in
  let nfaII = Re.parse lang2 |> Nfa.re_to_nfa in
  Nfa.prune nfaII;
  let lang1 = "a*" in
  let nfa1 = Re.parse lang1 |> Nfa.re_to_nfa in
  Nfa.prune nfa1;
  Nfa.merge_alphabets nfa1 nfaII;
  let nfa1 = Dfa.nfa_to_dfa nfa1 |> Dfa.complement |> Dfa.nfa_of_dfa in
  Nfa.merge_alphabets nfa1 nfaII;
  let nfa5 = Nfa.intersect nfa1 nfaII in
  Printf.printf "%b\n" (Nfa.is_empty nfa5);
  has_word nfa5 "";
  [%expect
    {|
    false
    accepting '': false |}]
;;
