{
  open Ckparse ;;
  exception Eoi ;;
  exception LexError of (Lexing.position * Lexing.position) ;;
}

let newline = ['\n']
let empty = [' ']

rule lex = parse
    | ("chicken")
      { Chicken }
    | newline
      { Eol }
    | eof 
      { Eof }
    | empty
      { lex lexbuf }
    | _     { raise (LexError (lexbuf.Lexing.lex_start_p,
                             lexbuf.Lexing.lex_curr_p)) }

{

}