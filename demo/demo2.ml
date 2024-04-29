open Regextkit

let () =
  let ast = Re.parse "b*" in
  let nfa = ast |> Regextkit.Nfa.re_to_nfa in
  Regextkit.Nfa.prune nfa;
  let _dfa = Regextkit.Dfa.nfa_to_dfa nfa in
  ()
;;
