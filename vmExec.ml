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
          VmBytecode.stack = (VmBytecode.VMV_int (j+i)) :: s }          
  | (_, ((VmBytecode.VMI_Sub) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_int j) :: s) ->
    (* Int constant. *)
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_int (j-i)) :: s }   
  | (_, ((VmBytecode.VMI_Mult) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_int j) :: s) ->
    (* Int constant. *)
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_int (j*i)) :: s }    
  | (_, ((VmBytecode.VMI_Compare) :: c), (VmBytecode.VMV_int i) :: (VmBytecode.VMV_int j) :: s) ->
    (* Int constant. *)
      let value = if i = j then 1 else 0 in
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = (VmBytecode.VMV_int value) :: s }    
  | (_, ((VmBytecode.VMI_Load) :: c), s) ->
    (* Int constant. *)
    let new_stack = in_comp s in
        { VmBytecode.register = VmBytecode.VMV_chicken "chicken";
          VmBytecode.code = c ;
          VmBytecode.stack = new_stack }    
;;
