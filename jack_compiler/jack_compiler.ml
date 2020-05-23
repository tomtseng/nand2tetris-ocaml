(** Executes compiler on input files. *)
open Core
open Stdio

(** Returns [input_filename], but with a ".vm" extension. *)
let get_output_filename (input_filename : string) : string =
  let
    base_filename, (_ : string option) = Filename.split_extension input_filename
  in
  base_filename ^ ".vm"

(** Returns list of input .jack files.

    If the input path is a .jack file, returns just that file. If the input path
    is a directory, returns all .jack files at the top level of that directory.
*)
let get_input_files (input_path : string) : string list =
  let is_jack_file (filename : string) : bool =
    match Filename.split_extension filename with
    | (_, Some extension) -> String.equal extension "jack"
    | (_, None) -> false
  in
  match Sys.is_directory input_path with
  | `Yes -> input_path |> Sys.readdir |> Array.filter ~f:is_jack_file
            |> Array.map ~f: (fun file ->
                String.concat [input_path ; "/" ; file ])
            |> Array.to_list
  | `No -> [ input_path ]
  | `Unknown ->
    failwith (Printf.sprintf "Can't determine file type: %s" input_path)

(** Prints the position information in [lexbuf] to [out_channel]. *)
let print_position
    (out_channel : Out_channel.t)
    (lexbuf : Lexing.lexbuf)
  : unit =
  let p = lexbuf.lex_curr_p in
  fprintf out_channel "%s:%d:%d" p.pos_fname p.pos_lnum
    (p.pos_cnum - p.pos_bol + 1)

(** Parses the contents of lexbuf, printing any error that appears. *)
let parse_with_error (lexbuf : Lexing.lexbuf) : Ast_types.class_declaration =
  try Parser.program Lexer.read lexbuf with
  | Lexer.Lexical_error err_msg ->
    fprintf stderr "%a: lexing error %s\n" print_position lexbuf err_msg;
    exit (-1)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

(** Runs the compiler on the input path. *)
let run_compiler (input_path : string) : unit =
  let input_files = get_input_files input_path in
  List.iter input_files ~f:(fun input_filename ->
      let lexbuf = Lexing.from_channel (In_channel.create input_filename) in
      lexbuf.lex_curr_p <-
        { lexbuf.lex_curr_p with pos_fname = input_filename };
      let jack_code = parse_with_error lexbuf in
      let vm_code = Compile.compile_program jack_code in
      let output_filename = get_output_filename input_filename in
      Out_channel.write_lines output_filename vm_code)

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Compiles Jack files to VM code."
    [%map_open
      let input_path = anon ("INPUT_PATH" %: string) in
      fun () -> run_compiler input_path
    ]

let () = Command.run command
