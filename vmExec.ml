exception Computation_success of VmBytecode.vm_val ;;
exception Computation_failure ;;



(* Print a state of the virtual machine. *)
let pp_state ppf state =
  Printf.fprintf ppf "Register r: %a\n%!"
    PrintByteCode.pp_value state.VmBytecode.register ;
  Printf.fprintf ppf "Code:\n%aEnd of code%!\n"
    PrintByteCode.pp_code state.VmBytecode.code ;
  Printf.fprintf ppf "Stack:\n" ;
  List.iter
    (fun v -> Printf.fprintf ppf "%a\n%!" PrintByteCode.pp_value v)
    state.VmBytecode.stack ;
  Printf.fprintf ppf "End of stack\n%!"
;;

let rec get_first list index = match list with 
  | h :: q -> if index = 0 then h else get_first q (index - 1)
  | [] -> raise Computation_failure

let rec in_load (stack : VmBytecode.vw_stack) index (new_stack : VmBytecode.vw_stack) = match index with 
  | 1 -> (get_first stack 0) :: (new_stack @ stack)
  | n -> match stack with 
          | [] -> raise Computation_failure
          | h :: q -> in_load q (n-1) (h :: new_stack)
;;

let rec in_store (stack : VmBytecode.vw_stack) value index (new_stack : VmBytecode.vw_stack) = match index with 
  | 1 -> new_stack @ ((VmBytecode.VMV_int value) :: stack)
  | n -> match stack with 
          | [] -> raise Computation_failure
          | h :: q -> in_store q value (n-1) (h :: new_stack)
;;

let rec in_jump code jump = match jump with 
  | 0 -> code 
  | n -> match code with 
          | [] -> raise Computation_failure
          | h :: q -> in_jump q (n - 1)
;;

(* Compute the next state of the machine from the current state. *)
let next_state state =
  match (state.VmBytecode.register,
         state.VmBytecode.code,
         state.VmBytecode.stack) with
  | (_, ((VmBytecode.VMI_Exit) :: c), s) ->
  (* Int constant. *)
      { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
        VmBytecode.code = [] ;
        VmBytecode.stack = s }
  | (_, ((VmBytecode.VMI_Chicken) :: c), s) ->
    (* Int constant. *)
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_chicken "chicken") :: s } 
  | (_, ((VmBytecode.VMI_Plus) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_int j) :: s) ->
    (* Int constant. *)
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_int (i+j)) :: s }
  | (_, ((VmBytecode.VMI_Plus) :: c), (VmBytecode.VMV_chicken str1) :: (VmBytecode.VMV_chicken str2) :: s) ->
    (* Int constant. *)
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_chicken (str1 ^ str2)) :: s }        
  | (_, ((VmBytecode.VMI_Plus) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_chicken str) :: s) ->
    (* Int constant. *)
        raise Computation_failure 
  | (_, ((VmBytecode.VMI_Plus) :: c), (VmBytecode.VMV_chicken str) :: (VmBytecode.VMV_int i) :: s) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Plus) :: c), _ :: []) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Plus) :: c), []) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Sub) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_int j) :: s) ->
    (* Int constant. *)
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_int (i-j)) :: s }
  | (_, ((VmBytecode.VMI_Sub) :: c), (VmBytecode.VMV_chicken str1) :: (VmBytecode.VMV_chicken str2) :: s) ->
    (* Int constant. *)
        raise Computation_failure    
  | (_, ((VmBytecode.VMI_Sub) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_chicken str) :: s) ->
    (* Int constant. *)
        raise Computation_failure 
  | (_, ((VmBytecode.VMI_Sub) :: c), (VmBytecode.VMV_chicken str) :: (VmBytecode.VMV_int i) :: s) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Sub) :: c), _ :: []) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Sub) :: c), []) ->
    (* Int constant. *)
        raise Computation_failure
  | (_, ((VmBytecode.VMI_Mult) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_int j) :: s) ->
    (* Int constant. *)
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_int (i*j)) :: s }
  | (_, ((VmBytecode.VMI_Mult) :: c), (VmBytecode.VMV_chicken str1) :: (VmBytecode.VMV_chicken str2) :: s) ->
    (* Int constant. *)
        raise Computation_failure       
  | (_, ((VmBytecode.VMI_Mult) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_chicken str) :: s) ->
    (* Int constant. *)
        raise Computation_failure 
  | (_, ((VmBytecode.VMI_Mult) :: c), (VmBytecode.VMV_chicken str) :: (VmBytecode.VMV_int i) :: s) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Mult) :: c), _ :: []) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Mult) :: c), []) ->
    (* Int constant. *)
        raise Computation_failure
  | (_, ((VmBytecode.VMI_Compare) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_int j) :: s) ->
    (* Int constant. *)
      let value = if i = j then 1 else 0 in
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_int value) :: s }
  | (_, ((VmBytecode.VMI_Compare) :: c), (VmBytecode.VMV_chicken str1) :: (VmBytecode.VMV_chicken str2) :: s) ->
    (* Int constant. *)
      let value = if str1 = str2 then 1 else 0 in
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_int value) :: s }
  | (_, ((VmBytecode.VMI_Compare) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_chicken str) :: s) ->
    (* Int constant. *)
        raise Computation_failure 
  | (_, ((VmBytecode.VMI_Compare) :: c), (VmBytecode.VMV_chicken str) :: (VmBytecode.VMV_int i) :: s) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Compare) :: c), _ :: []) ->
    (* Int constant. *)
        raise Computation_failure     
  | (_, ((VmBytecode.VMI_Compare) :: c), []) ->
    (* Int constant. *)
        raise Computation_failure    
  | (_, ((VmBytecode.VMI_Load) :: instr :: c), (VmBytecode.VMV_int indx) :: s) ->
    (* Int constant. *)
      let new_stack = in_load ((VmBytecode.VMV_int indx) :: s) indx [] in  
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = new_stack }  
  | (_, ((VmBytecode.VMI_Load) :: []), _) ->
    (* Int constant. *)
      raise Computation_failure
  | (_, ((VmBytecode.VMI_Load) :: _), (VmBytecode.VMV_chicken str) :: s) ->
    (* Int constant. *)
      raise Computation_failure
  | (_, ((VmBytecode.VMI_Load) :: _), []) ->
    (* Int constant. *)
      raise Computation_failure
  | (_, ((VmBytecode.VMI_Store) :: instr :: c), (VmBytecode.VMV_int indx) :: s) ->
    (* Int constant. *)
      let value = match instr with 
        | VmBytecode.VMI_Exit -> 0
        | VmBytecode.VMI_Chicken -> 1
        | VmBytecode.VMI_Plus -> 2
        | VmBytecode.VMI_Sub -> 3
        | VmBytecode.VMI_Mult -> 4
        | VmBytecode.VMI_Compare -> 5 
        | VmBytecode.VMI_Load -> 6
        | VmBytecode.VMI_Store -> 7
        | VmBytecode.VMI_Jump -> 8
        | VmBytecode.VMI_Char -> 9
        | VmBytecode.VMI_Push n -> 10+n
      in let new_stack = in_store ((VmBytecode.VMV_int indx) :: s) value indx [] in  
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = new_stack } 
    | (_, ((VmBytecode.VMI_Store) :: []), _) ->
      raise Computation_failure
    | (_, ((VmBytecode.VMI_Store) :: _), (VmBytecode.VMV_chicken str) :: s) ->
      raise Computation_failure
    | (_, ((VmBytecode.VMI_Store) :: _), []) ->
      raise Computation_failure
    | (_, ((VmBytecode.VMI_Jump) :: c), (VmBytecode.VMV_int nb_jump) :: (VmBytecode.VMV_int condition) :: s) ->
      (* Int constant. *)
        let new_code = match condition with 
            | 0 -> c 
            | _ -> in_jump c nb_jump 
            in
          { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
            VmBytecode.code = new_code ;
            VmBytecode.stack = (VmBytecode.VMV_int nb_jump) :: (VmBytecode.VMV_int condition) :: s }  
    | (_, ((VmBytecode.VMI_Jump) :: c), (VmBytecode.VMV_chicken str) :: _) ->
      raise Computation_failure 
    | (_, ((VmBytecode.VMI_Jump) :: c), (VmBytecode.VMV_int i) :: []) ->
      raise Computation_failure  
    | (_, ((VmBytecode.VMI_Jump) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_chicken str) :: _) ->
      raise Computation_failure  
    | (_, ((VmBytecode.VMI_Jump) :: c), []) ->
      raise Computation_failure  
    | (_, ((VmBytecode.VMI_Char) :: c), (VmBytecode.VMV_int num) :: s) ->
      (* Int constant. *) 
          { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
            VmBytecode.code = c ;
            VmBytecode.stack = (VmBytecode.VMV_chicken (Char.escaped (Char.chr num))) :: s } 
    | (_, ((VmBytecode.VMI_Char) :: c), (VmBytecode.VMV_chicken str) :: s) ->
      raise Computation_failure
    | (_, ((VmBytecode.VMI_Char) :: c), []) ->
      raise Computation_failure
    | (_, ((VmBytecode.VMI_Push n) :: c), s) ->
      (* Int constant. *) 
          { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
            VmBytecode.code = c ;
            VmBytecode.stack = (VmBytecode.VMV_int n) :: s } 
    | (r, [(* Return *)], s) ->
        (* No more code to execute : the end. *)
        raise (Computation_success (get_first s 0))
;;
