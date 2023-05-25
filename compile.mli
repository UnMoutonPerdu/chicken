exception Unbound_identifier of string

type object_code = {
  global : VmBytecode.vm_code;
  funs : (string * VmBytecode.vm_code) list;
}
val compile_program : string list -> Impast.toplevel list -> object_code

type executable = {
  entry_point : VmBytecode.vm_code;
  funs_codes : (string * VmBytecode.vm_code) list;
}
val make_executable : object_code -> executable
