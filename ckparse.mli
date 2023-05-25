type token =
  | Eof
  | Eol
  | Chicken

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ckast.program
