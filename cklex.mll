{
  type token = EOF | EOL | Chicken
}

let newline = ['\n']
let empty = [' ']

rule lex var = parse
    | ("chicken")
      { lex (var @ [Chicken]) lexbuf }
    | newline
      { lex (var @ [EOL]) lexbuf }
    | eof 
      { var @ [EOF] }
    | empty
      { lex var lexbuf }

{

}