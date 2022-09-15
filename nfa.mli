type state = int
type nfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

(* |construct_nfa| -- constructs an nfa from input regex *)
val construct_nfa : Ast.re -> nfa

(* |merge_alphabets| -- returns an nfa with the alphabet unioned with another nfa *)
val merge_alphabets : nfa -> nfa -> nfa