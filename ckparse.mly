%{
open Ckast ;;
%}

%token Eof
%token Eol
%token Chicken

%start program
%type <Ckast.program> program

%%

program:
| Eof { EOL :: [] }
| chicken program { $1 :: $2 }
;

chicken:
| Chicken { CHICKEN }
| Eol     { EOL }
;
