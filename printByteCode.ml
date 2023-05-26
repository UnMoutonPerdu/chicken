let rec __pp_code ppf depth code = List.iter (__pp_instr ppf depth) code

and __pp_instr ppf depth instr =
  (* Print indentation. *)
  let curr_indent = String.make (2 * depth) ' ' in
  Printf.fprintf ppf "%s" curr_indent ;
  match instr with
  | VmBytecode.VMI_Exit -> Printf.fprintf ppf "EXIT !\n" 
  | VmBytecode.VMI_Load -> Printf.fprintf ppf "Load !\n" 
  | VmBytecode.VMI_Plus -> Printf.fprintf ppf "Plus !\n"
  | VmBytecode.VMI_Sub -> Printf.fprintf ppf "Sub !\n"
  | VmBytecode.VMI_Mult -> Printf.fprintf ppf "Mult !\n"
  | VmBytecode.VMI_Compare -> Printf.fprintf ppf "Compare !\n"
  | VmBytecode.VMI_Store -> Printf.fprintf ppf "Store !\n"
  | VmBytecode.VMI_Jump -> Printf.fprintf ppf "Jump !\n"
  | VmBytecode.VMI_Char -> Printf.fprintf ppf "Char !\n"
  | VmBytecode.VMI_Chicken -> Printf.fprintf ppf "Chicken !\n"
  | VmBytecode.VMI_Push n -> Printf.fprintf ppf "Push %d!\n" n
;;


let rec pp_separated_list ppf printer = function
  | [] -> ()
  | [last] -> Printf.fprintf ppf "%a" printer last
  | h :: q ->
      Printf.fprintf ppf "%a, " printer h ;
      pp_separated_list ppf printer q
;;


(* Print a virtual machine value. *)
let rec pp_value ppf = function
  (* | VmBytecode.VMV_int i -> Printf.fprintf ppf "%d" i
  | VmBytecode.VMV_bool b -> Printf.fprintf ppf "%b" b
  | VmBytecode.VMV_string s -> Printf.fprintf ppf "%s" s
  | VmBytecode.VMV_closure (_body, _env) ->
      Printf.fprintf ppf "<fun>"
      (* Otherwise, for full print, cumbersome...
      Printf.fprintf ppf "<fun (" ;
      pp_separated_list ppf (fun _ppf -> (__pp_instr _ppf 0)) body ;
      Printf.fprintf ppf ", %a)>" pp_value env
       *)
  | VmBytecode.VMV_env vals ->
      Printf.fprintf ppf "<env> = [" ;
      pp_separated_list ppf pp_value vals ;
      Printf.fprintf ppf "]"
  | VmBytecode.VMV_code_addr _ -> Printf.fprintf ppf "<code>" *)
  | _ -> ()
;;


(* Print a virtual machine bytecode program. Only exported function
   to hide the extra indentation parameter that must always be 0 at
   the initiall call. *)
let pp_code ppf code = __pp_code ppf 0 code ;;
