exception Unbound_identifier of string ;;


(* Find the position of an element in a list, with position 0 being the
   head of the list. *)
let rec find_pos x = function
  | [] -> raise Not_found
  | h :: q -> if h = x then 0 else 1 + (find_pos x q)
;;


(* �valuation d'une expression. La m�moire est implicite puisque globale. *)
let rec compile_expr rho = function
  | Impast.Int i -> [ VmBytecode.VMI_Loadi i ]
  | Impast.Bool b -> [ VmBytecode.VMI_Loadb b ]
  | Impast.String s -> [ VmBytecode.VMI_Loads s ]
  | Impast.Ident id ->
      let pos =
        (try find_pos id rho with Not_found -> raise (Unbound_identifier id)) in
      [VmBytecode.VMI_Read pos]
  | Impast.ArrayRead (id, e_index) ->
      let pos_array = (try find_pos id rho with Not_found -> raise (Unbound_identifier id)) in 
      let index = compile_expr rho e_index in 
      [VmBytecode.VMI_Read pos_array; VmBytecode.VMI_Push] @ index @ [VmBytecode.VMI_Indxread]
  | Impast.App (f_name, args) -> compile_app rho f_name args
  | Impast.Monop (o_name, e) -> (
      match o_name with
      | "-" ->
          (* Translate to 0 - e. *)
          let translation = Impast.Binop ("-", (Impast.Int 0), e) in
          compile_expr rho translation
      | _ -> raise (Failure "Unknown monop")
     )
  | Impast.Binop (o_name, e1, e2) ->
      (* Get the virtual machine instruction depending on the operator. *)
      let binop_instr =
        (match o_name with
        | "+" -> VmBytecode.VMI_Plus
        | "-" -> VmBytecode.VMI_Sub
        | "*" -> VmBytecode.VMI_Mult
        | "/" -> VmBytecode.VMI_Div
        | "==" -> VmBytecode.VMI_Equal
        | "<" -> VmBytecode.VMI_Lt
        | "<=" -> VmBytecode.VMI_Le
        | ">" -> VmBytecode.VMI_Gt
        | ">=" -> VmBytecode.VMI_Ge
        | _ -> raise (Failure "Unknown binop")) in
      (compile_expr rho e1) @ [VmBytecode.VMI_Push] @ (compile_expr rho e2) @ [binop_instr]

and compile_app_args rho args = match args with 
        | [] -> []
        | h :: t -> [VmBytecode.VMI_Loadi 1; VmBytecode.VMI_Mkblock; VmBytecode.VMI_Envext] @ (compile_expr ("" :: rho) h) @ [VmBytecode.VMI_Assign 0] @ compile_app_args ("" :: rho) t

and compile_app rho f_name args =
  let mkblocks = compile_app_args rho args in 
  [VmBytecode.VMI_Pushenv] @ mkblocks @ [VmBytecode.VMI_Call f_name; VmBytecode.VMI_Popenv]

and compile_instr rho = function
  | Impast.While (e, i) ->
      let ecode = compile_expr rho e in 
      let icode = compile_instr rho i in 
      ecode @ [VmBytecode.VMI_Loop (ecode, icode)]
  | Impast.If (e, i1, i2) ->
      let e_code = compile_expr rho e in
      let i1_code = compile_instr rho i1 in
      let i2_code = compile_instr rho i2 in
       e_code @ [VmBytecode.VMI_Branch (i1_code, i2_code)]
  | Impast.Assign (id, e) ->
      let pos = (try find_pos id rho with Not_found -> raise (Unbound_identifier id)) in 
        (compile_expr rho e) @ [VmBytecode.VMI_Assign pos]
  | Impast.Seq (i1, i2) ->
      (compile_instr rho i1) @ (compile_instr rho i2)
  | Impast.Return e_opt ->
      let e_code =
        (match e_opt with None -> [] | Some e -> compile_expr rho e) in
      e_code @ [VmBytecode.VMI_Return]
  | Impast.Iapp (f_name, args) -> compile_app rho f_name args
  | Impast.Print args -> (
      match args with 
        | [] -> []
        | h :: t -> (compile_expr rho h) @ [VmBytecode.VMI_Print] @ (compile_instr rho (Impast.Print t))
  )
  | Impast.ArrayWrite (id, e1, e2) ->
      let pos_array = (try find_pos id rho with Not_found -> raise (Unbound_identifier id)) in 
      let e1_code = compile_expr rho e1 in 
      let e2_code = compile_expr rho e2 in 
      [VmBytecode.VMI_Read pos_array; VmBytecode.VMI_Push] @ e2_code @ [VmBytecode.VMI_Push] @ e1_code @ [VmBytecode.VMI_Indxwrite]


and compile_var_decls rho decls =
  match decls with 
    | [] -> (rho, [])
    | (name, decl) :: t -> let code = (
      match decl with 
        | Impast.Scalar -> [VmBytecode.VMI_Loadi 1] @ [VmBytecode.VMI_Mkblock] @ [VmBytecode.VMI_Envext]
        | Impast.Array e -> 
          let ecode = compile_expr rho e in 
          let ptr = [VmBytecode.VMI_Push; VmBytecode.VMI_Loadi 1; VmBytecode.VMI_Mkblock; VmBytecode.VMI_Envext] in 
          let alloc_arr = [VmBytecode.VMI_Pop; VmBytecode.VMI_Mkblock; VmBytecode.VMI_Assign 0] in 
          ecode @ ptr @ alloc_arr
              ) in 
        let (rho', instr) = compile_var_decls (name :: rho) t 
          in (rho', code @ instr) 
;;


type object_code = {
  (* D�finitions globales toplevel � ex�cuter au tout d�but du programme. *)
  global : VmBytecode.vm_code ;
  (* Code de chacune des fonctions. *)
  funs : (string * VmBytecode.vm_code) list
} ;;


(* Nature du bytecode g�n�r�. Le code provenant de d�clarations de variables
   globales est regroup� en d�but de bytecode. *)
type toplevel_code =
  | TC_Global of VmBytecode.vm_code
  | TC_Fun of (string * VmBytecode.vm_code)
;;


let compile_toplevel rho = function
  | Impast.Vardecl v_decl ->
      let (rho', var_code) =  compile_var_decls rho [v_decl] in
       (rho', (TC_Global var_code))
  | Impast.Fundef f_def ->
      (* Penser � inverser la list des param�tres car on les empile dans
         l'environnement. *)
      let rho' = (List.rev f_def.Impast.params) @ rho in
      let (rho'', vars_code) = compile_var_decls rho' f_def.Impast.vars in
      (* Compilation du corps de la fonction. *)
      let body_code = compile_instr rho'' f_def.Impast.body in
      (* On joint le code des d�clarations de variables locales et celui du
         corps de la fonction. *)
      let all_code = vars_code @ body_code in
      (* L'environnement reste inchang� au final car les fonctions ne sont pas
         li�es dans l'environnement. *)
      (rho, (TC_Fun (f_def.Impast.f_name, all_code)))
;;


let rec compile_program rho = function
  | [] -> { global = [] ; funs = [] }
  | h :: q -> (
      let (rho', code) = compile_toplevel rho h in
      let rem_code = compile_program rho' q in
      match code with
      | TC_Global c -> { global = c @ rem_code.global ; funs = rem_code.funs }
      | TC_Fun c -> { global = rem_code.global ; funs = c :: rem_code.funs }
     )
;;


(* Ex�cutable final, avec le point d'entr�e identifi� et la liste de toutes
   les fonctions du programme avec leur code.
   Le point d'entr�e correspond au code des d�clarations de variables globales
   suivi du code de main. *)
type executable = {
  entry_point : VmBytecode.vm_code ;
  funs_codes : (string * VmBytecode.vm_code) list
} ;;


let make_executable obj_code =
  (* On recherche le code du main. *)
  let main_code =
    (try List.assoc "main" obj_code.funs with
    | Not_found -> raise (Failure "Main not defined")) in
  let whole_code = obj_code.global @ main_code in
  { entry_point = whole_code ; funs_codes = obj_code.funs }
;;
