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
     Nfa.create [ 0; 1; 2 ] [ "b" ] [ 0, "b", 1; 1, "ε", 0; 2, "ε", 0 ] 2 [ 0; 1 ]
     in *)
  Printf.printf "%b\n" (Nfa.is_empty nfa1);
  (* print_endline (Nfa.export_graphviz nfa1); *)
  [%expect {| false |}];
  (* let nfa2 = Re.parse "bb*" |> Nfa.re_to_nfa in *)
  (* let nfa2 = Tree.(Concat (Literal "b", Star (Literal "b"))) |> Nfa.re_to_nfa in
     Nfa.prune nfa2; *)
  let nfa2 =
    Nfa.create
      [ 0; 1; 2; 3; 4 ]
      [ "b" ]
      [ 0, "b", 1; 1, "ε", 4; 4, "ε", 2; 3, "ε", 2; 2, "b", 3 ]
      0
      [ 1; 3 ]
  in
  Printf.printf "%b\n" (Nfa.is_empty nfa2);
  print_endline (Nfa.export_graphviz nfa2);
  [%expect
    {|
    false
    digraph G {
     n0 [label="", shape=none, height=0, width=0, ]
    "0" [shape=ellipse, ];
    "1" [shape=ellipse, peripheries=2, ];
    "2" [shape=ellipse, ];
    "3" [shape=ellipse, peripheries=2, ];
    "4" [shape=ellipse, ];

    n0 -> 0;
    1 -> 4 [label="ε", ];
    0 -> 1 [label="b", ];
    4 -> 2 [label="ε", ];
    3 -> 2 [label="ε", ];
    2 -> 3 [label="b", ];

    } |}];
  (* let nfa1 = Nfa.create [ 1 ] [ "b" ] [ 1, "ε", 1; 1, "b", 1 ] 1 [ 1 ] in
     Printf.printf "%b\n" (Nfa.is_empty nfa1);
     let nfa2 = Nfa.create [ 0; 1 ] [ "b" ] [ 0, "b", 1; 1, "ε", 1; 1, "b", 1 ] 0 [ 1 ] in
     Printf.printf "%b\n" (Nfa.is_empty nfa2); *)
  let nfa3 = Nfa.intersect ~verbose:true nfa1 nfa2 in
  [%expect
    {|
    add (2,2) -> (0,2) with "b" II
    add (2,3) -> (0,2) with "\206\181" I
    add (2,4) -> (0,2) with "\206\181" I
    add (2,0) -> (0,0) with "b" II
    add (2,1) -> (0,4) with "\206\181" I
    add (0,2) -> (1,3) with "b" I
    add (0,3) -> (0,2) with "b" III
    add (0,4) -> (0,2) with "b" III
    add (0,0) -> (1,1) with "b" I
    add (0,1) -> (0,4) with "b" III
    add (1,2) -> (0,2) with "b" II
    add (1,3) -> (0,2) with "\206\181" I
    add (1,4) -> (0,2) with "\206\181" I
    add (1,0) -> (0,0) with "b" II
    add (1,1) -> (0,4) with "\206\181" I |}];
  Nfa.prune nfa3;
  print_endline (Nfa.export_graphviz nfa3);
  Printf.printf "empty intersection: %b\n" (Nfa.is_empty nfa3);
  [%expect {|
    digraph G {
     n0 [label="", shape=none, height=0, width=0, ]
    "1" [shape=ellipse, ];
    "5" [shape=ellipse, ];
    "3" [shape=ellipse, ];
    "11" [shape=ellipse, ];
    "7" [shape=ellipse, peripheries=2, ];
    "9" [shape=ellipse, peripheries=2, ];

    n0 -> 11;
    1 -> 7 [label="b", ];
    11 -> 1 [label="ε", ];
    9 -> 3 [label="ε", ];
    5 -> 3 [label="ε", ];
    3 -> 9 [label="b", ];
    7 -> 5 [label="ε", ];

    }
    empty intersection: false |}];
  assert (Nfa.is_accepted nfa3 "b");
  [%expect {||}];
  Printf.printf "accepting 'bb': %b\n" (Nfa.is_accepted nfa3 "bb");
  (* BUG *)
  [%expect {|

    accepting 'bb': true
      |}]
;;

let%expect_test _ =
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

let%expect_test "NFA intersection" =
  let lang1 = "(ka)*du((screams)+(sings))(ka)*du*" in
  let lang2 = "kakadu((says)+(sings)+(screams))(loudly)?kaka(ka)*duu*" in
  let lang1 = "k*" in
  let lang2 = "kk" in
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

let%expect_test "negations via dfa" =
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

let%expect_test "Intersection with complement (via DFA)" =
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
