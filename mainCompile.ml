let version = "0.01" ;;

let usage () =
  let _ =
    Printf.eprintf
      "Usage: %s [file]\n\tRead an Imp program from file (default is stdin) and compile it to bytecode\n%!"
    Sys.argv.(0) in
  exit 1
;;


let main () =
  let input_channel =
    match Array.length Sys.argv with
    | 1 -> stdin
    | 2 -> (
        match Sys.argv.(1) with
        | "-" -> stdin
        | name -> (
            try open_in name with
              _ -> Printf.eprintf "Opening %s failed\n%!" name; exit 1
           )
       )
    | n -> usage () in
  let lexbuf = Lexing.from_channel input_channel in
  let _ = Printf.printf "        Welcome to IMP, version %s\n%!" version in
  try
    let _ = Printf.printf  "> %!" in
    let prgm = Impparse.program Implex.lex lexbuf in
    let _ = Impast.print_program stdout prgm in
    let _ = Printf.fprintf stdout " :\n%!" in
    let obj_code = Compile.compile_program [] prgm in
    let exe = Compile.make_executable obj_code in
    Printf.printf "GLOBAL SECTION\n%a"
      PrintByteCode.pp_code obj_code.Compile.global ;
    Printf.printf "FUNCTIONS SECTION\n" ;
    List.iter
      (fun (f_name, f_code) ->
        Printf.printf "%% %s:\n%a\n" f_name PrintByteCode.pp_code f_code)
      obj_code.Compile.funs ;
    let out_channel = open_out_bin "a.out" in
    output_value out_channel exe ;
    close_out out_channel ;
    Printf.printf  "Bye.\n%!" ; exit 0
  with
  | Implex.Eoi -> Printf.printf "Unexpected en of file.\n%!" ; exit 1
  | Failure msg -> Printf.printf "Error: %s\n%!" msg ; exit 2
  | Parsing.Parse_error -> Printf.printf "Syntax error\n%!" ; exit 3
;;

if !Sys.interactive then () else main () ;;
