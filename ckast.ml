type chicken =
  | CHICKEN 
  | EOL
;;

type program = chicken list ;;


open Printf ;;

let print_toplevel oc = function
  | CHICKEN -> fprintf oc "CHICKEN! "
  | EOL     -> fprintf oc "EndOfLine!\n"
;;

let print_program oc prgm = List.iter (print_toplevel oc) prgm ;;

