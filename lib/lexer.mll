{
open Parser
}

let white = [' ' '\t']+
let ident = ['a'-'z' 'A'-'Z' '0'-'9']

rule token = 
    parse
          white                   { token lexbuf }
        | "+"                     { UNION }
        | "*"                     { STAR }
        | "\\."                   { IDENT "." }
        | "."                     { CONCAT }
        | "("                     { LPAR }
        | ")"                     { RPAR }
        | "EMPTY" | "∅"           { EMPTY }
        | "EPSILON" | "ε" | "?"   { EPSILON }
        | ident                   { IDENT (Lexing.lexeme lexbuf) }
        | eof                     { EOF }
        | _                       { BADTOK }
