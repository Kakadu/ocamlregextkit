(* |parse| -- parse a string as a regular expression *)
let parse s =
    let lexbuf = Lexing.from_string s in
    try
        Parser.regex Lexer.token lexbuf
    with 
        Parsing.Parse_error -> 
            let tok = Lexing.lexeme lexbuf in
            Printf.printf "[ERROR] Syntax Error at token '%s'\n" tok;
            exit 1

(* |main| -- main read-print loop *)
let main () =
    let dflag = ref false and
        vflag = ref false and
        fns = ref [] and
        usage = "Usage: regextkit [-option] \"<regex>\" \"<regex>\"" in
    Arg.parse [
        ("-v", Arg.Set vflag, " Output stages");
        ("-d", Arg.Set dflag, " Print the tree");
    ] (fun s -> fns := !fns @ [s]) usage;
    if List.length !fns <> 2 then (
        Printf.eprintf "%s\n" usage;
        exit 2;
    );
    let reg1 = List.hd !fns and
        reg2 = List.nth !fns 1 in
    try
        let starttime = Sys.time() in
        if !vflag then print_string "Constructing ASTs...\t";
        let re = parse reg1 in
        let simp_re = Ast.simplify re in
        let re2 = parse reg2 in
        let simp_re2 = Ast.simplify re2 in
        let asttime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (asttime -. starttime);
        if !dflag then (
            Print.print_ast simp_re;
            print_newline ();
            Print.print_ast simp_re2;
            print_newline ();
        );

        if !vflag then print_string "Constructing NFAs...\t";
        let nfa = Nfa.construct_nfa simp_re and
            nfa2 = Nfa.construct_nfa simp_re2 in

        (* need to correct each nfa's alphabets to be the union, otherwise we 
            have an issue where the compliment of one language is the same as the compliment of another
            (e.g. with languages EPSILON,  a* ) *)
        let new_nfa = Nfa.merge_alphabets nfa nfa2 and
            new_nfa2 = Nfa.merge_alphabets nfa2 nfa in
        let nfatime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (nfatime -. asttime);
        if !dflag then (
            Print.print_nfa new_nfa;
            print_newline ();
            Print.print_nfa new_nfa2;
            print_newline ();
        );

        if !vflag then print_string "Constructing DFAs...\t";
        let dfa = Dfa.nfa_to_dfa new_nfa and
            dfa2 = Dfa.nfa_to_dfa new_nfa2 in
        let dfatime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (dfatime -. nfatime);

        if !vflag then print_string "Reducing DFAs...\t";
        let reduced_dfa = Dfa.reduce_dfa dfa and
            reduced_dfa2 = Dfa.reduce_dfa dfa2 in
        let reddfatime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (reddfatime -. dfatime);
        if !dflag then (
            Print.print_dfa reduced_dfa;
            print_newline ();
            Print.print_dfa reduced_dfa2;
            print_newline ();
        );

        if !vflag then print_string "Comparing DFAs...\t";
        let dfaword = Dfa.is_dfa_equal reduced_dfa reduced_dfa2 in
        let finaltime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (finaltime -. reddfatime);
        match dfaword with
            | Some word -> Printf.printf "'%s' exists in one regex but not the other\n" word;
            | None -> print_string "Input regex are equal\n";

        exit 0;
        
    with
        | e ->
            let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            Printf.eprintf "[ERROR] %s%s\n" msg stack;
            raise e
  
let regex = main ()

(* TODO: Uses too much memory if large input (>18 NFA states)  
    -- this issue is acutally caused by List.map (which was applied to the result). 
    Since List.map is not tail recursive, the stack space is proportional to the length of the input list (exponential to our NFA states in our case)
    this problem persists for all usages of List.map. experiment using List.rev_map instead?
    see more here https://discuss.ocaml.org/t/a-new-list-map-that-is-both-stack-safe-and-fast/865/2

    issue exemplified by the following strings:
        ? (1+0.1)*.(0+?)
        ? (1*.0.1.1* ).(0+?)+1*.(0+?)

        caused by stack overflow from powerset (and from typing each set as State)
*)