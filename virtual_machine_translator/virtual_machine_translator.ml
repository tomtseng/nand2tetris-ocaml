open Core
open Stdio

(** State of virtual-machine-code-to-hack-assembly translator. *)
module TranslatorState : sig
  type t

  (** [create base_filename] Initializes translator state for translating a file
      with base name [base_filename]. *)
  val create : string -> t

  (** [update t command] Updates translator state for processing next command
      [command]. *)
  val update : t -> Ast_types.command -> t

  (** Get name of current file being translated. *)
  val get_filename : t -> string

  (** Get name of current function being translated. *)
  val get_function_name : t -> string

  (** Get unique index of command in file being translated.*)
  val get_command_number : t -> int
end = struct
  type t = {
    base_filename : string;
    function_name : string;
    command_number : int;
  }

  let create filename =
    { base_filename = filename ; function_name = "" ; command_number = 0 ; }

  let update state _ =
    { state with command_number = state.command_number + 1 }

  let get_filename state = state.base_filename
  let get_function_name state = state.function_name
  let get_command_number state = state.command_number
end

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
    (state : TranslatorState.t)
    (command : Ast_types.comparison_command)
  : string list =
  let filename = TranslatorState.get_filename state in
  let command_number = TranslatorState.get_command_number state in
  let comparison_true_label =
    Printf.sprintf "$$%s.comparison_%d_true" filename command_number
  in
  let comparison_end_label =
    Printf.sprintf "$$%s.comparison_%d_end" filename command_number
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
    (state : TranslatorState.t)
    (location : Ast_types.memory_location)
  : string list =
  let filename = TranslatorState.get_filename state in
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
    (state : TranslatorState.t)
    (location : Ast_types.memory_location)
  : string list =
  (translate_retrieve_address state location) @
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
    (state : TranslatorState.t)
    (location : Ast_types.memory_location)
  : string list =
  (* Assembly commands to fetch the contents of the memory location into
     register D. *)
  let retrieve_to_d_register_assembly =
    match location.segment with
    | Argument | Local | Static | This | That | Pointer | Temp ->
      (translate_retrieve_address state location) @ [ "D=M" ]
    | Constant -> [ Printf.sprintf "@%d" location.index ; "D=A" ]
  in
  retrieve_to_d_register_assembly @
  [
    "@SP" ;
    "M=M+1" ;
    "A=M-1" ;
    "M=D" ;
  ]

(** Translate label string to its assembly form. *)
let translate_label (state : TranslatorState.t) (label : string) : string =
  let func_name = TranslatorState.get_function_name state in
  Printf.sprintf "%s$%s" func_name label

(** Translate label command. *)
let translate_label_command (state : TranslatorState.t) (label : string)
  : string list =
  [ Printf.sprintf "(%s)" (translate_label state label) ]

(** Translate goto command. *)
let translate_goto (state : TranslatorState.t) (label : string) : string list =
  [ Printf.sprintf "@%s" (translate_label state label) ; "0;JMP" ]

(** Translate if-goto command *)
let translate_if_goto (state : TranslatorState.t) (label : string) : string list =
  [
    (* Pop from top of stack into register D. *)
    "@SP" ;
    "M=M-1" ;
    "A=M" ;
    "D=M" ;

    Printf.sprintf "@%s" (translate_label state label) ;
    "D;JNE" ;
  ]

(** Translates virtual machine commands to Hack assembly code. *)
let translate_commands
    (state : TranslatorState.t)
    (commands : Ast_types.command list)
  : string list =
  let rec translate_commands_impl
      (current_state : TranslatorState.t)
      (remaining_commands : Ast_types.command list)
      (acc : string list list)
    : string list =
    match remaining_commands with
    | [] -> acc |> List.rev |> List.concat
    | command :: tail_commands ->
      let next_state = TranslatorState.update current_state command in
      let command_translation =
        match command with
        | Pop location -> translate_pop next_state location
        | Push location -> translate_push next_state location
        | Binary_expression e -> translate_binary_expression e
        | Unary_expression e -> translate_unary_expression e
        | Comparison c -> translate_comparison next_state c
        | Label label -> translate_label_command next_state label
        | Goto label -> translate_goto next_state label
        | If_goto label -> translate_if_goto next_state label
        | Function _
        | Call _
        | Return -> failwith "not implemented"
      in
      translate_commands_impl next_state tail_commands (command_translation :: acc)
  in
  translate_commands_impl state commands []

(** Translates file to Hack assembly. *)
let translate_file (filename : string) : string list =
  let file_commands = get_file_commands filename in
  let base_filename = Filename.basename filename in
  let state = TranslatorState.create base_filename in
  translate_commands state file_commands

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
