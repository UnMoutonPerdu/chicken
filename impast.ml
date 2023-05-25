type expr =
  | Int of int
  | Bool of bool
  | String of string
  | Ident of string
  | ArrayRead of (string * expr)
  | App of (string * (expr list))
  | Monop of (string * expr)
  | Binop of (string * expr * expr)


and instr =
  | While of (expr * instr)
  | If of (expr * instr * instr)
  | Assign of (string * expr)
  | ArrayWrite of (string * expr * expr)
  | Seq of (instr * instr)
  | Return of (expr option)
  | Iapp of (string * (expr list))
  | Print of expr list


and var_decl =
  | Scalar
  | Array of expr
;;


type fun_def = {
  f_name : string ;
  params : string list ;
  vars : (string * var_decl) list ;
  body : instr
} ;;


type toplevel =
  | Vardecl of (string * var_decl)
  | Fundef of fun_def
;;

type program = toplevel list ;;


open Printf ;;


let rec print_expr oc = function
  | Int n -> fprintf oc "%d" n
  | Bool b -> fprintf oc "%s" (if b then "T" else "F")
  | Ident s -> fprintf oc "%s" s
  | String s -> fprintf oc "\"%s\"" (String.escaped s)
  | App (f_name, params) -> fprintf oc "%s (%a)" f_name print_exprs params
  | Binop (op, e1, e2) ->
      fprintf oc "(%a %s %a)" print_expr e1 op print_expr e2
  | Monop (op, e) -> fprintf oc "%s%a" op print_expr e
  | ArrayRead (s, e) -> fprintf oc "%s[%a]" s print_expr e


and print_exprs oc = function
  | [] -> ()
  | [last] -> print_expr oc last
  | h :: q -> fprintf oc "%a, %a" print_expr h print_exprs q


and print_var_decl oc = function
  | (v_name, Scalar) -> fprintf oc "var %s ;\n" v_name
  | (v_name, (Array e)) -> fprintf oc "var %s[%a] ;\n" v_name print_expr e
;;


let rec print_instr oc = function
  | While (e, i) ->
      fprintf oc "while %a do\n%adone\n" print_expr e print_instr i
  | If (e, i1, i2) ->
      fprintf oc "if %a then %aelse %aendif;\n"
        print_expr e print_instr i1 print_instr i2
  | Assign (id, e) -> fprintf oc "%s := %a ;\n" id print_expr e
  | ArrayWrite (id, e1, e2) ->
      fprintf oc "%s[%a] := %a ;\n" id print_expr e1 print_expr e2
  | Seq (i1, i2) -> fprintf oc "%a%a" print_instr i1 print_instr i2
  | Return e_op -> (
      match e_op with
      | None ->fprintf oc "return ;\n"
      | Some e -> fprintf oc "return %a ;\n" print_expr e
     )
  | Iapp (f_name, params) -> fprintf oc "%s (%a) ;\n" f_name print_exprs params
  | Print params -> fprintf oc "print (%a) ;\n" print_exprs params
;;


let print_var_decls oc v_names = List.iter (print_var_decl oc) v_names ;;

let rec print_param_decls oc = function
  | [] -> ()
  | [last] -> fprintf oc "%s" last
  | h :: q -> fprintf oc "%s, %a" h print_param_decls q
;;


let print_toplevel oc = function
  | Vardecl decl -> print_var_decl oc decl
  | Fundef f_def ->
      fprintf oc "%s (%a)\nbegin\n%a%aend\n"
        f_def.f_name print_param_decls f_def.params print_var_decls f_def.vars
        print_instr f_def.body
;;


let print_program oc prgm = List.iter (print_toplevel oc) prgm ;;

