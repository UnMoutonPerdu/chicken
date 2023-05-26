type vm_code = vm_instr list

and vm_instr =
  | VMI_Exit
  | VMI_Load
  | VMI_Plus | VMI_Sub
  | VMI_Mult 
  | VMI_Compare
  | VMI_Store
  | VMI_Jump 
  | VMI_Char
  | VMI_Push of int
  | VMI_Chicken
;;

type vm_val =
  | VMV_int of int
  | VMV_chicken of string
;;

type vw_stack = vm_val list ;;

type vm_state = { register : vm_val ; code : vm_code ; stack : vw_stack} ;;
