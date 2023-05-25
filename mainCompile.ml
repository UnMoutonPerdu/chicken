let version = "0.01" ;;

let usage () =
  let _ =
    Printf.eprintf
      "Usage: %s [file]\n\tRead a CHICKEN program from file (default is stdin) \
       Compiles CHICKEN into bytecode.\nTakes input from \
       standard input ended by ^D or from a file passed as argument.\n \
       Bytecode file is always named \'a.out'.\n%!"
    Sys.argv.(0) in
  exit 1
;;


let main () =
  let input_channel =
    match Array.length Sys.argv with
    | 1 -> stdin
    | 2 ->
        begin match Sys.argv.(1) with
        | "-" -> stdin
        | name ->
            begin try open_in name with
              _ -> Printf.eprintf "Opening %s failed\n%!" name; exit 1
            end
        end
    | n -> usage () in
  let out_channel = open_out_bin "a.out" in
  let compiled_sentences = ref ([] : VmBytecode.vm_code list) in
  let lexbuf = Lexing.from_channel input_channel in
  let _ = Printf.printf "        Welcome to CHICKEN, version %s\n%!" version in
  while true do
    try
      let _ = Printf.printf  "> %!" in
      let e = Ckparse.program Cklex.lex lexbuf in
      let code = Compile.compile_expr [] e in
      Printf.printf "%a" PrintByteCode.pp_code code ;
      (* Stored in reverse order for sake of efficiency. *)
      compiled_sentences := code :: !compiled_sentences ;
      Printf.printf "\n%!"
    with
    | Cklex.Eoi ->
        (* Ok, right, job is done. We still just need to write the bycode
           file. No need to reverse first the list of the compiled sentences,
           List.fold_left will do this for us by the way. *)
        let whole_code =
          List.fold_left
            (fun accu code -> code @ accu) [] !compiled_sentences in
        output_value out_channel whole_code ;
        close_out out_channel ;
        Printf.printf  "Bye.\n%!" ; exit 0
    | Failure msg -> Printf.printf "Error: %s\n\n" msg
    | Compile.Unbound_identifier id ->
        Printf.printf "Unbound identifier %s.@." id
    | Compile.Compilation_not_implemented ->
        Printf.printf "Compilation not yet implemented@."
    | Parsing.Parse_error ->
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        Format.printf
          "File %S, line %i, characters %i-%i: Syntax error.\n"
          sp.Lexing.pos_fname
          sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
    | Cklex.LexError (sp, ep) ->
        Printf.printf
          "File %S, line %i, characters %i-%i: Lexical error.\n"
          sp.Lexing.pos_fname
          sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
  done
;;

if !Sys.interactive then () else main () ;;
