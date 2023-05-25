type token =
  | Eof
  | Eol
  | Chicken

open Parsing;;
let _ = parse_error;;
# 2 "ckparse.mly"
open Ckast ;;
# 11 "ckparse.ml"
let yytransl_const = [|
  257 (* Eof *);
  258 (* Eol *);
  259 (* Chicken *);
    0|]

let yytransl_block = [|
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\004\000\003\000\005\000\000\000\002\000"

let yydgoto = "\002\000\
\006\000\007\000"

let yysindex = "\002\000\
\255\254\000\000\000\000\000\000\000\000\000\000\255\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\253\255\000\000"

let yytablesize = 4
let yytable = "\003\000\
\004\000\005\000\001\000\008\000"

let yycheck = "\001\001\
\002\001\003\001\001\000\007\000"

let yynames_const = "\
  Eof\000\
  Eol\000\
  Chicken\000\
  "

let yynames_block = "\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 15 "ckparse.mly"
      ( EOL :: [] )
# 64 "ckparse.ml"
               : Ckast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'chicken) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ckast.program) in
    Obj.repr(
# 16 "ckparse.mly"
                  ( _1 :: _2 )
# 72 "ckparse.ml"
               : Ckast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 20 "ckparse.mly"
          ( CHICKEN )
# 78 "ckparse.ml"
               : 'chicken))
; (fun __caml_parser_env ->
    Obj.repr(
# 21 "ckparse.mly"
          ( EOL )
# 84 "ckparse.ml"
               : 'chicken))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ckast.program)
