%{
open Impast ;;
%}

%token EOF
%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token <string> STRING
%token COLONEQUAL
%right COLONEQUAL
%token WHILE DO DONE BEGIN END RETURN VAR
%token PLUS MINUS MULT DIV EQUALEQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%token LPAR RPAR SEMICOLON COMMA LBRACKET RBRACKET
%token IF THEN ELSE ENDIF PRINT
%left EQUALEQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%left PLUS MINUS
%left MULT DIV

%start program
%type <Impast.program> program

%%

program:
| EOF { [] }
| toplevel program { $1 :: $2 }
;

toplevel:
| var_decl { Vardecl $1 }
| fun_def { Fundef $1 }
;

var_decl:
| VAR IDENT SEMICOLON { ($2, Scalar) }
| VAR IDENT LBRACKET expr RBRACKET SEMICOLON { ($2, (Array $4)) }
;

opt_var_decls:
| { [] }
| var_decl opt_var_decls { $1 :: $2 }
;

fun_def:
| IDENT LPAR opt_params RPAR BEGIN opt_var_decls instrs END
    { { f_name = $1 ; params = $3 ; vars = $6 ; body = $7 } }
;

opt_params:
| { [] }
| IDENT params { $1 :: $2 }
;

params:
| { [] }
| COMMA IDENT params { $2 :: $3 }
;

instrs:
| instr { $1 }
| instr instrs { Seq ($1, $2) }
;

instr:
| IF expr THEN instrs ELSE instrs ENDIF
    { If ($2, $4, $6) }
| WHILE expr DO instrs DONE
    { While ($2, $4) }
| IDENT COLONEQUAL expr SEMICOLON
    { Assign ($1, $3) }
| IDENT LBRACKET expr RBRACKET COLONEQUAL expr SEMICOLON
    { ArrayWrite ($1, $3, $6) }
| BEGIN instrs END
    { $2 }
| RETURN opt_expr SEMICOLON
    { Return $2 }
| IDENT LPAR opt_exprs RPAR SEMICOLON
    { Iapp ($1, $3) }
| PRINT LPAR opt_exprs RPAR SEMICOLON
    { Print $3 }
;

expr:
| IDENT LPAR opt_exprs RPAR      { App ($1, $3) }
| expr EQUALEQUAL expr           { Binop ("==", $1, $3) }
| expr GREATER expr              { Binop (">", $1, $3) }
| expr GREATEREQUAL expr         { Binop (">=", $1, $3) }
| expr SMALLER expr              { Binop ("<", $1, $3) }
| expr SMALLEREQUAL expr         { Binop ("<=", $1, $3) }
| expr PLUS expr                 { Binop ("+", $1, $3) }
| expr MINUS expr                { Binop ("-", $1, $3) }
| expr MULT expr                 { Binop ("*", $1, $3) }
| expr DIV expr                  { Binop ("/", $1, $3) }
| MINUS expr                     { Monop ("-", $2) }
| LPAR expr RPAR                 { $2 }
| atom                           { $1 }
| IDENT LBRACKET expr RBRACKET   { ArrayRead ($1, $3) }
;

 opt_expr:
| { None }
| expr               { Some $1 }
;

opt_exprs:
| { [] }
| expr exprs               { $1 :: $2 }
;

exprs:
| { [] }
| COMMA expr exprs         { $2 :: $3 }
;

atom:
| INT            { Int ($1) }
| TRUE           { Bool (true) }
| FALSE          { Bool (false) }
| STRING         { String ($1) }
| IDENT          { Ident ($1) }
;
