exception Computation_success of VmBytecode.vm_val ;;
exception Computation_failure ;;

(* Là où aller chercher le code des fonctions lors des appels.
   Initialisé uen fosi pour toute lorsque le bytecode est chargé. *)
let all_funs = ref ([] : (string * VmBytecode.vm_code) list) ;;


(* Print a state of the virtual machine. *)
let pp_state ppf state =
  Printf.fprintf ppf "------\n" ;
  Printf.fprintf ppf "Register r: %a\n%!"
    PrintByteCode.pp_value state.VmBytecode.register ;
  Printf.fprintf ppf "Code:\n%aEnd of code%!\n"
    PrintByteCode.pp_code state.VmBytecode.code ;
  Printf.fprintf ppf "Stack:\n" ;
  List.iter
    (fun v -> Printf.fprintf ppf " %a%!" PrintByteCode.pp_value v)
    state.VmBytecode.stack ;
  Printf.fprintf ppf "\nEnd of stack\n%!" ;
  Printf.fprintf ppf "Env:\n" ;
  List.iter
    (fun addr -> Printf.fprintf ppf " @%i%!" addr) state.VmBytecode.env ;
  Printf.fprintf ppf "\nEnd of env\n%!" ;
  Printf.fprintf ppf "Mem:\n" ;
  for i = 0 to Mem.mem.VmBytecode.next_free - 1 do
    Printf.fprintf ppf " @%i: %a%!"
      i PrintByteCode.pp_value Mem.mem.VmBytecode.data.(i)
  done ;
  Printf.fprintf ppf "\nEnd of mem\n%!" ;
;;


(* Compute the next state of the machine from the current state. *)
let next_state state =
  match (state.VmBytecode.register,
         state.VmBytecode.code,
         state.VmBytecode.stack) with
  | (_, ((VmBytecode.VMI_Loadi i) :: c), s) ->
      (* Int constant. *)
      { VmBytecode.register = VmBytecode.VMV_int i ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (_, ((VmBytecode.VMI_Loadb b) :: c), s) ->
      (* Bool constant. *)
      { VmBytecode.register = VmBytecode.VMV_bool b ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (_, ((VmBytecode.VMI_Loads str) :: c), s) ->
      (* String constant. *)
      { VmBytecode.register = VmBytecode.VMV_string str ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Plus :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Addition. *)
       { VmBytecode.register = VmBytecode.VMV_int (j + i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Sub :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Subtraction. *)
       { VmBytecode.register = VmBytecode.VMV_int (j - i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Mult :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Multiplication. *)
       { VmBytecode.register = VmBytecode.VMV_int (j * i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Div :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Division.
          Same remark about non-commutativity of division than for
          subtraction. *)
       { VmBytecode.register = VmBytecode.VMV_int (j / i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Equal :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Comparison = assumed to be between 2 integers. *)
       { VmBytecode.register = VmBytecode.VMV_bool (i = j) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool i), (VmBytecode.VMI_Equal :: c),
     ((VmBytecode.VMV_bool j):: s')) ->
       (* Comparison = assumed to be between 2 booleans. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j = i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_string str1), (VmBytecode.VMI_Equal :: c),
     ((VmBytecode.VMV_string str2):: s')) ->
       (* Comparison = assumed to be between 2 strings. *)
       { VmBytecode.register = VmBytecode.VMV_bool (str2 = str1) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Lt :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Comparison < assumed to be between 2 integers. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j < i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool i), (VmBytecode.VMI_Lt :: c),
     ((VmBytecode.VMV_bool j):: s')) ->
       (* Comparison < assumed to be between 2 booleans. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j < i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_string str1), (VmBytecode.VMI_Lt :: c),
     ((VmBytecode.VMV_string str2):: s')) ->
       (* Comparison < assumed to be between 2 strings. *)
       { VmBytecode.register = VmBytecode.VMV_bool (str2 < str1) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
   | ((VmBytecode.VMV_int i), (VmBytecode.VMI_Gt :: c),
     ((VmBytecode.VMV_int j):: s')) ->
       (* Comparison > assumed to be between 2 integers. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j > i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool i), (VmBytecode.VMI_Gt :: c),
     ((VmBytecode.VMV_bool j):: s')) ->
       (* Comparison > assumed to be between 2 booleans. *)
       { VmBytecode.register = VmBytecode.VMV_bool (j > i) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_string str1), (VmBytecode.VMI_Gt :: c),
     ((VmBytecode.VMV_string str2):: s')) ->
       (* Comparison > assumed to be between 2 strings. *)
       { VmBytecode.register = VmBytecode.VMV_bool (str2 > str1) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s' ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool true), ((VmBytecode.VMI_Branch (c1, _)) :: c),
     s) ->
       (* Branch if true. *)
      { VmBytecode.register = state.VmBytecode.register ;
	      VmBytecode.code = c1 @ c ;
	      VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool false), ((VmBytecode.VMI_Branch (_, c2)) :: c),
     s) ->
       (* Branch if false. *)
      { VmBytecode.register = state.VmBytecode.register ;
	      VmBytecode.code = c2 @ c ;
	      VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (r, (VmBytecode.VMI_Push :: c), s) ->
      (* Push onto the stack. *)
      { VmBytecode.register = r ;
	      VmBytecode.code = c ;
        VmBytecode.stack = r :: s ;
        VmBytecode.env = state.VmBytecode.env }
  | (_, (VmBytecode.VMI_Pop :: c), v :: s) ->
      (* Pop from the stack. *)
      { VmBytecode.register = v ;
	      VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (_, ((VmBytecode.VMI_Read n) :: c),
     s) ->
       (* Read identifier. *)
       let addr =
         (try List.nth state.VmBytecode.env n with
         | Failure ("nth") ->
             raise
               (Failure ("VMI_Read out of env : " ^ (string_of_int n)))) in
       if addr >= Mem.mem.VmBytecode.size then
         raise (Failure "Access out of memory") ;
       { VmBytecode.register = Mem.mem.VmBytecode.data.(addr) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int bsize), (VmBytecode.VMI_Mkblock :: c),
     s) ->
       (* Allocation dynamique. Prend la taille du bloc à allouer dans
          le registre et met l'adresse allouée dans le registre. *)
       let addr = Mem.new_block bsize in
      { VmBytecode.register = VmBytecode.VMV_addr addr ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_addr addr), (VMI_Envext :: c),
     s) ->
       (* Extension de l'environnement. Prend l'adresse à lier dans le
          registe.*)
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = addr :: state.VmBytecode.env }
  | (r, ((VmBytecode.VMI_Assign n) :: c),
     s) ->
       (* Affectation d'une variable. Prend la valeur à affecter dans le
          registre. *)
       let addr =
         (try List.nth state.VmBytecode.env n with
         | Failure ("nth") ->
             raise
               (Failure ("VMI_Assing out of env : " ^ (string_of_int n)))) in
       if addr >= Mem.mem.VmBytecode.size then
         raise (Failure "Access out of memory") ;
       Mem.mem.VmBytecode.data.(addr) <- r ;
       { VmBytecode.register = r ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | (r, (VmBytecode.VMI_Print :: c),
     s) ->
       (* Affichage. Prend la valeur à afficher dans le registre. *)
       Printf.printf "%a" PrintByteCode.pp_value r ;
       { VmBytecode.register = r ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool true),
     ((VmBytecode.VMI_Loop (code_cond, code_body)) :: c),
     s) ->
       (* Boucle lorsque la condition est vraie. *)
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code =
           code_body @ code_cond @ [VmBytecode.VMI_Loop (code_cond, code_body)]
           @ c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_bool false),
     ((VmBytecode.VMI_Loop (_, _)) :: c),
     s) ->
       (* Boucle lorsque la condition est vraie. *)
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | (_, ((VmBytecode.VMI_Call f_name) :: c),
     s) ->
       (* Appel de fonction. Push le code restant sur la pile avant de sauter
          dans le code de la fonction. *)
       let f_code =
         (try List.assoc f_name !all_funs with
         | Not_found -> raise (Failure ("Undefined function " ^ f_name))) in
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code = f_code ;
         VmBytecode.stack = (VMV_code_addr c) :: s ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int index), (VmBytecode.VMI_Indxread :: c),
     (VmBytecode.VMV_addr base_addr) :: s) ->
       (* Lecture dans un tableau. L'indice où aller lire est dans
          le registre. L'adresse de base du tableau est au sommet de la pile. *)
       if base_addr + index >= Mem.mem.VmBytecode.size then
         raise (Failure "Access out of memory") ;
       { VmBytecode.register = Mem.mem.VmBytecode.data.(base_addr + index) ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = state.VmBytecode.env }
  | ((VmBytecode.VMV_int index), (VmBytecode.VMI_Indxwrite :: c),
     v :: (VmBytecode.VMV_addr base_addr) :: s) ->
       (* Affectation dans une case de tableau. L'indice est dans le registre
          et la valeur à affectée est prise au sommet de la pile et
          l'adresse de base du tableau juste en dessous. *)
       if base_addr +index >= Mem.mem.VmBytecode.size then
         raise (Failure "Access out of memory") ;
       Mem.mem.VmBytecode.data.(base_addr + index) <- v ;
        { VmBytecode.register = state.VmBytecode.register ;
          VmBytecode.code = c ;
          VmBytecode.stack = s ;
          VmBytecode.env = state.VmBytecode.env }
  | (_, ((VmBytecode.VMI_Return) :: c),
     s) ->
       (* Retour de fonction. On ignore tout le code restant à exécuter.
          La VM ira cherher le code placé en attente sur la pile lors de
          l'appel de la fonction courante que l'on quitte.. *)
       { VmBytecode.register = state.VmBytecode.register ;
         VmBytecode.code = [] ;
         VmBytecode.stack = state.VmBytecode.stack ;
         VmBytecode.env = state.VmBytecode.env }
  | (r, (VmBytecode.VMI_Le :: c),
     s) -> failwith "TODO Le"
  | (r, (VmBytecode.VMI_Ge :: c),
     s) -> failwith "TODO Ge"
  | (r, (VmBytecode.VMI_Pushenv :: c),
     s) ->
       (* Sauvegarde de l'environnement sur la pile. *)
      { VmBytecode.register = r ;
        VmBytecode.code = c ;
        VmBytecode.stack = (VmBytecode.VMV_env state.VmBytecode.env) :: s ;
        VmBytecode.env = state.VmBytecode.env }
  | (r, VmBytecode.VMI_Popenv :: c,
     ((VmBytecode.VMV_env e) :: s)) ->
       (* Restoration de l'environnement depuis la pile. *)
       { VmBytecode.register = r ;
         VmBytecode.code = c ;
         VmBytecode.stack = s ;
         VmBytecode.env = e }
  | (r, [(* Return *)], ((VmBytecode.VMV_code_addr c) :: s)) ->
      (* Return from function call, continue pending code. *)
      { VmBytecode.register = r ;
        VmBytecode.code = c ;
        VmBytecode.stack = s ;
        VmBytecode.env = state.VmBytecode.env }
  | (r, [(* Return *)], []) ->
      (* No more code to execute : the end. *)
      raise (Computation_success r)
  | (_, _, _) ->
      Printf.eprintf "State trace:\n%a%!" pp_state state ;
      raise Computation_failure
;;
