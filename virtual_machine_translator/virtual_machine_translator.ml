open Core
open Stdio

(** Returns `input_filename`, but with a ".asm" extension. *)
let get_asm_output_filename (input_filename : string) : string =
  let
    base_filename, (_ : string option) = Filename.split_extension input_filename
  in
  base_filename ^ ".asm"

(** Returns list of input .vm files.
 *
 *  If the input path is a .vm file, returns just that file If the input path is
 *  a directory, returns all .vm files at the top level of that directory. *)
let get_input_files (input_path : string) : string list =
  let is_vm_file (filename : string) : bool =
    match Filename.split_extension filename with
    | (_, Some extension) -> String.equal extension "vm"
    | (_, None) -> false
  in
  match Sys.is_directory input_path with
  | `Yes -> input_path |> Sys.readdir |> Array.filter ~f:is_vm_file
            |> Array.map ~f:(fun file -> String.concat [input_path ; "/" ; file ])
            |> Array.to_list
  | `No -> [ input_path ]
  | `Unknown -> failwith (Printf.sprintf "Can't determine file type: %s" input_path)

(** Print the position information in `lexbuf` to `out_channel`. *)
let print_position (out_channel : Stdio.Out_channel.t) (lexbuf : Lexing.lexbuf) : unit =
  let p = lexbuf.lex_curr_p in
  fprintf out_channel "%s:%d:%d" p.pos_fname p.pos_lnum
    (p.pos_cnum - p.pos_bol + 1)

(** Parse the contents of lexbuf, printing any error that appears. *)
let parse_with_error (lexbuf : Lexing.lexbuf) : Ast_types.command list =
  try Parser.program Lexer.read lexbuf with
  | Parser.Error ->
    let stack = Printexc.get_backtrace () in
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    fprintf stderr "%s" stack;
    exit (-1)

(** Parse file into virtual machine commands. *)
let get_file_commands (input_filename : string) : Ast_types.command list =
  let lexbuf = Lexing.from_channel (In_channel.create input_filename) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_filename };
  parse_with_error lexbuf

let assembly_preamble =
  [
    (* Initialize stack pointer *)
    "@256" ;
    "D=A" ;
    "@SP";
    "M=D" ;
  ]

(* Translate Add, Subtract, Bitwise_and, Bitwise_or *)
let translate_binary_expression (expr : Ast_types.binary_expression) : string list =
  let operation =
    match expr with
    | Add -> "D+M"
    | Subtract -> "M-D"
    | Bitwise_and -> "D&M"
    | Bitwise_or -> "D|M"
  in
  [
    "@SP" ;
    "A=M-1" ;
    "D=M" ;
    "A=A-1" ;
    (* now D = [first element of stack], M = [second element of stack]. *)
    Printf.sprintf "M=%s" operation ;
    (* Decrement stack pointer *)
    "@SP" ;
    "M=M-1" ;
  ]

(* Translate Negative, Bitwise_not *)
let translate_unary_expression (expr : Ast_types.unary_expression) : string list =
  let operation =
    match expr with
    | Negative -> "-M"
    | Bitwise_not -> "!M"
  in
  [
    "@SP" ;
    "A=M-1" ;
    Printf.sprintf "M=%s" operation ;
  ]

(* Translate Equals, Greater_than, Less_than *)
let translate_comparison
    (filename : string)
    (command_number : int)
    (command : Ast_types.comparison_command)
  : string list =
  let comparison_true_label =
    Printf.sprintf "%s.comparison_%d_true" filename command_number
  in
  let comparison_end_label =
    Printf.sprintf "$%s.comparison_%d_end" filename command_number
  in
  let jump =
    match command with
    | Equals -> "JEQ"
    | Greater_than -> "JGT"
    | Less_than -> "JLT"
  in
  [
    "@SP" ;
    "A=M-1" ;
    "D=M" ;
    "A=A-1" ;
    "D=M-D" ;
    (* now D = [second element of stack - first element of stack] *)
    Printf.sprintf "@%s" comparison_true_label ;
    Printf.sprintf "D;%s" jump ;
    "D=0" ;
    Printf.sprintf "@%s" comparison_end_label ;
    "0;JMP" ;
    Printf.sprintf "(%s)" comparison_true_label ;
    "D=-1" ;
    Printf.sprintf "(%s)" comparison_end_label ;
    (* now D = result of comparison *)

    (* Decrement stack pointer *)
    "@SP" ;
    "M=M-1" ;

    (* Update element at top of stack *)
    "A=M-1" ;
    "M=D" ;
  ]

(** Generate assembly instructions that place the memory location in register A.
    Register D's contents may be erased in the process. *)
let translate_retrieve_address
    (filename : string)
    (location : Ast_types.memory_location)
  : string list =
  match location.segment with
  | Argument | Local | This | That ->
    let register =
      match location.segment with
      | Argument -> "ARG"
      | Local -> "LCL"
      | This -> "THIS"
      | That -> "THAT"
      | Pointer | Temp | Static | Constant ->
        failwith "unexpected memory segment"
    in
    [
      Printf.sprintf "@%s" register ;
      "D=M" ;
      Printf.sprintf "@%d" location.index ;
      "A=D+A" ;
    ]
  | Pointer -> [ Printf.sprintf "@%d" (3 + location.index) ]
  | Temp -> [ Printf.sprintf "@%d" (5 + location.index) ]
  | Static ->  [ Printf.sprintf "@%s.%d" filename location.index ]
  | Constant -> failwith "unexpected memory segment"

(** Translate push command. *)
let translate_pop
    (filename : string)
    (location : Ast_types.memory_location)
  : string list =
  (translate_retrieve_address filename location) @
  [
    (* Store memory address in scratch register. *)
    "D=A" ;
    "@R13" ;  (* scratch register *)
    "M=D" ;

    (* Pop stack. *)
    "@SP" ;
    "M=M-1" ;
    "A=M" ;
    "D=M";

    (* Store result in memory address. *)
    "@R13" ;
    "A=M" ;
    "M=D" ;
  ]

(** Translate push command. *)
let translate_push
    (filename : string)
    (location : Ast_types.memory_location)
  : string list =
  (* Assembly commands to fetch the contents of the memory location into
     register D. *)
  let retrieve_to_d_register_assembly =
    match location.segment with
    | Argument | Local | Static | This | That | Pointer | Temp ->
      (translate_retrieve_address filename location) @ [ "D=M" ]
    | Constant -> [ Printf.sprintf "@%d" location.index ; "D=A" ]
  in
  retrieve_to_d_register_assembly @
  [
    "@SP" ;
    "M=M+1" ;
    "A=M-1" ;
    "M=D" ;
  ]


(** [translate filename command_number command] Translates virtual machine
    command [command] to Hack assembly code.

    [filename] should be a unique identifier for the file that [command] appears
    in, and [command_number] should be unique for each command in the file. *)
let translate
    (filename : string)
    (command_number : int)
    (command : Ast_types.command)
  : string list =
  match command with
  | Pop location -> translate_pop filename location
  | Push location -> translate_push filename location
  | Binary_expression e -> translate_binary_expression e
  | Unary_expression e -> translate_unary_expression e
  | Comparison c -> translate_comparison filename command_number c

(** Translates file to Hack assembly. *)
let translate_file (filename : string) : string list =
  let file_commands = get_file_commands filename in
  let base_filename = Filename.basename filename in
  List.concat_mapi file_commands ~f:(translate base_filename)

(** Runs the VM translator on the input path. *)
let run_translator (input_path : string) : unit =
  let input_files = get_input_files input_path in
  let output_filename = get_asm_output_filename input_path in
  let assembly =
    List.append assembly_preamble List.(input_files >>= translate_file)
  in
  Out_channel.write_lines output_filename assembly

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Translates a VM program to Hack assembly code."
    [%map_open
      let input_path = anon ("INPUT_PATH" %: string) in
      fun () -> run_translator input_path
    ]

let () = Command.run command
