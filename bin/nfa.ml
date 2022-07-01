open Ast

type state = int
type nfa = {
  states: state list; transitions: (state * string * state) list; start: state; accepting: state list
}

let counter = ref 0;;

(* |construct_nfa| -- recursively builds NFA for given re *)
let rec construct_nfa re = 
    match re with
          Literal a -> counter := !counter + 2; 
                    {states = [!counter - 2; !counter - 1]; start = !counter - 2; accepting = [!counter - 1]; transitions = [(!counter - 2, a, !counter - 1)]}
        | Epsilon -> counter := !counter + 1;
                    {states = [!counter - 1]; start = !counter - 1; accepting = [!counter - 1]; transitions = []}
        | Empty -> counter := !counter + 1;
                    {states = [!counter - 1]; start = !counter - 1; accepting = []; transitions = []}
        | Union (r1, r2) -> let nfa1 = construct_nfa r1 and nfa2 = construct_nfa r2 in
                    counter := !counter + 1;
                    {states = (!counter - 1) :: (nfa1.states @ nfa2.states); start = !counter - 1; accepting = nfa1.accepting @ nfa2.accepting; 
                    transitions = ((!counter - 1, "ε", nfa1.start) :: nfa1.transitions) @ ((!counter - 1, "ε", nfa2.start) :: nfa2.transitions)}
        | Concat (r1, r2) -> let nfa1 = construct_nfa r1 and nfa2 = construct_nfa r2 in
                    let newtrans = List.map (fun s -> (s,"ε",nfa2.start)) nfa1.accepting in
                    {states = nfa1.states @ nfa2.states; start = nfa1.start; accepting = nfa2.accepting;
                    transitions = nfa1.transitions @ newtrans @ nfa2.transitions}
        | Star r -> let nfa1 = construct_nfa r in
                    let newtrans = List.map (fun s -> (s, "ε", nfa1.start)) nfa1.accepting in
                    counter := !counter + 1;
                    {states = (!counter - 1) :: nfa1.states; start = !counter - 1; accepting = (!counter - 1) :: nfa1.accepting;
                    transitions = (!counter - 1, "ε", nfa1.start) :: newtrans}

(* |print_nfa| -- prints out nfa representation *)
let print_nfa n = 
    print_string "states: "; List.iter (fun s -> print_int s; print_char ' ') n.states; print_newline ();
    print_string "start: "; print_int n.start; print_newline ();
    print_string "accepting: "; List.iter (fun s -> print_int s; print_char ' ') n.accepting; print_newline ();
    print_string "transitions: "; print_newline(); List.iter (fun (s,a,t) -> print_string "    "; print_int s; print_string ("\t--"^a^"-->\t"); print_int t; print_newline ()) n.transitions;
