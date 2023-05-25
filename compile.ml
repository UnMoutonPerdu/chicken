exception Compilation_not_implemented ;;
exception Unbound_identifier of string ;;

let rec find_pos x rho = match rho with 
    | h :: t -> if h = x then 0 else 1 + find_pos x t
    | [] -> raise Not_found

let rec chicken_to_int e counter code = match e with 
  | h :: q -> (match h with 
                | Ckast.CHICKEN -> chicken_to_int q (counter + 1) code
                | Ckast.EOL     -> chicken_to_int q 0 (code @ [counter]))
  | [] -> code

let rec int_to_code e code = match e with 
  | [] -> code
  | h :: q -> (match h with 
                | 0 -> int_to_code q (code @ [EXIT]) 
                )
(* Generate bytecode for a general expression. *)
let rec compile_expr rho e = 
  let ecode = chicken_to_int e 0 [] in 
    match ecode with 
;;
