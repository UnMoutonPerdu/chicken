{
  open Ckparse ;;
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

{

}