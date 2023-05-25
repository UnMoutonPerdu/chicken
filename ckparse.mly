%{
open Ckast ;;
%}

%token EOF
%token EOL
%token Chicken

%start program
%type <Ckast.program> program

%%

program:
| EOF { [] }
| chicken program { $1 :: $2 }
;

chicken:
| Chicken { Vardecl $1 }
;
