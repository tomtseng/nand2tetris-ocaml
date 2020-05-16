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

  (** Gets name of current file being translated. *)
  val get_filename : t -> string

  (** Gets name of current function being translated. *)
  val get_function_name : t -> string

  (** Gets unique index of command in file being translated.*)
  val get_command_number : t -> int
end = struct
  type t = {
    base_filename : string;
    function_name : string;
    command_number : int;
  }

  let create filename =
    { base_filename = filename ; function_name = "" ; command_number = 0 ; }

  let update state command =
    let incremented_state =
      { state with command_number = state.command_number + 1 }
    in
    match command with
    | Ast_types.Function (func_name, _) ->
      { incremented_state with function_name = func_name }
    | Pop _ | Push _ | Binary_expression _ | Unary_expression _ | Comparison _
    | Label _ | Goto _ | If_goto _ | Call _ | Return -> incremented_state

  let get_filename state = state.base_filename
  let get_function_name state = state.function_name
  let get_command_number state = state.command_number
end

(** Returns name of output `.asm` file. *)
let get_asm_output_filename (input_path : string) : string =
  match Sys.is_directory input_path with
  | `Yes ->
    let base_filename = Filename.basename input_path in
    String.concat [input_path ; "/" ; base_filename ; ".asm"]
  | `No ->
    let extensionless_file_path, (_ : string option) =
      Filename.split_extension input_path
    in
    extensionless_file_path ^ ".asm"
  | `Unknown -> failwith (Printf.sprintf "Can't determine file type: %s" input_path)

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

(** Prints the position information in `lexbuf` to `out_channel`. *)
let print_position (out_channel : Stdio.Out_channel.t) (lexbuf : Lexing.lexbuf) : unit =
  let p = lexbuf.lex_curr_p in
  fprintf out_channel "%s:%d:%d" p.pos_fname p.pos_lnum
    (p.pos_cnum - p.pos_bol + 1)

(** Parses the contents of lexbuf, printing any error that appears. *)
let parse_with_error (lexbuf : Lexing.lexbuf) : Ast_types.command list =
  try Parser.program Lexer.read lexbuf with
  | Parser.Error ->
    let stack = Printexc.get_backtrace () in
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    fprintf stderr "%s" stack;
    exit (-1)

(** Parses file into virtual machine commands. *)
let get_file_commands (input_filename : string) : Ast_types.command list =
  let lexbuf = Lexing.from_channel (In_channel.create input_filename) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_filename };
  parse_with_error lexbuf

(** Concatenate list of assembly of commands into a single string.*)
let concat_commands : string list -> string = String.concat ~sep:"\n"

(** Assembly commands that push the contents of D register onto the stack. *)
let push_d_register_onto_stack : string =
  concat_commands
    [
      "@SP" ;
      "M=M+1" ;
      "A=M-1" ;
      "M=D" ;
    ]

(** Assembly commands that pop the top of the stack into the D register. *)
let pop_stack_to_d_register : string =
  concat_commands
    [
      "@SP" ;
      "M=M-1" ;
      "A=M" ;
      "D=M";
    ]

(** Constructs label. Labels of different "types", specified by
    type_description, will not conflict so long as no distinct type is a prefix
    of another. *)
let get_label (type_description : string) (arguments : string list) : string =
  Printf.sprintf "%s$%s" type_description (String.concat ~sep:"$" arguments)

(** Gets the assembly label for label command. *)
let get_label_label (state : TranslatorState.t) (label : string) : string =
  let function_name = TranslatorState.get_function_name state in
  get_label "label" [function_name ; label]

(** Gets the assembly label for a function with name [func_name]. *)
let get_function_label (function_name : string) : string =
  get_label "func" [function_name]

(** Gets the label for a static variable. *)
let get_static_label
    (state : TranslatorState.t)
    (memory_location_index : int)
  : string =
  get_label
    "static"
    [ TranslatorState.get_filename state ; string_of_int memory_location_index ]

(** Gets the label to return to after a function call. *)
let get_return_label (state: TranslatorState.t) : string =
  let filename = TranslatorState.get_filename state in
  let command_number = TranslatorState.get_command_number state |> string_of_int in
  get_label "return" [filename ; command_number]

(** Translates Add, Subtract, Bitwise_and, Bitwise_or *)
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

(** Translate Negative, Bitwise_not *)
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

(** Translates Equals, Greater_than, Less_than *)
let translate_comparison
    (state : TranslatorState.t)
    (command : Ast_types.comparison_command)
  : string list =
  let filename = TranslatorState.get_filename state in
  let command_number = TranslatorState.get_command_number state |> string_of_int in
  let comparison_true_label = get_label "comp_true" [filename ; command_number] in
  let comparison_end_label = get_label "comp_end" [filename ; command_number] in
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

(** Generates assembly instructions that place the memory location in register A.
    Register D's contents may be erased in the process. *)
let translate_retrieve_address
    (state : TranslatorState.t)
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
  | Static ->  [ Printf.sprintf "@%s" (get_static_label state location.index) ]
  | Constant -> failwith "unexpected memory segment"

(** Translates push command. *)
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

    pop_stack_to_d_register ;

    (* Store result in memory address. *)
    "@R13" ;
    "A=M" ;
    "M=D" ;
  ]

(** Translates push command. *)
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
  retrieve_to_d_register_assembly @ [push_d_register_onto_stack]

(** Translates label command. *)
let translate_label_command (state : TranslatorState.t) (label : string)
  : string list =
  [ Printf.sprintf "(%s)" (get_label_label state label) ]

(** Translates goto command. *)
let translate_goto (state : TranslatorState.t) (label : string) : string list =
  [ Printf.sprintf "@%s" (get_label_label state label) ; "0;JMP" ]

(** Translates if-goto command *)
let translate_if_goto (state : TranslatorState.t) (label : string) : string list =
  [
    pop_stack_to_d_register ;
    Printf.sprintf "@%s" (get_label_label state label) ;
    "D;JNE" ;
  ]

(** Translates function declaration with name [func_name] and
    [num_local_variables] local variables. *)
let translate_function
    (function_name : string)
    (num_local_variables : int)
  : string list =
  let initialize_local_variables =
    "D=0"
    :: (List.init num_local_variables ~f:(fun _ -> push_d_register_onto_stack))
  in
  (Printf.sprintf "(%s)" (get_function_label function_name))
  :: initialize_local_variables

(** Translates function call with name [func_name] and [num_args] arguments
    already pushed. *)
let translate_call
    (state : TranslatorState.t)
    (function_name : string)
    (num_arguments : int)
  : string list =
  let return_label = get_return_label state in
  let save_segment_on_stack (segment : string) : string =
    concat_commands
      [
        Printf.sprintf "@%s" segment ;
        "D=M" ;
        push_d_register_onto_stack
      ]
  in
  [
    (* Save return address on stack. *)
    Printf.sprintf "@%s" return_label ;
    "D=A" ;
    push_d_register_onto_stack ;

    (* Save memory segments on stack. *)
    save_segment_on_stack "LCL" ;
    save_segment_on_stack "ARG" ;
    save_segment_on_stack "THIS" ;
    save_segment_on_stack "THAT" ;

    (* Reposition ARG pointer. *)
    Printf.sprintf "@%d" (num_arguments + 5) ;
    "D=A" ;
    "@SP" ;
    "D=M-D" ;
    "@ARG" ;
    "M=D" ;

    (* Reposition LCL pointer. *)
    "@SP" ;
    "D=M" ;
    "@LCL" ;
    "M=D" ;

    (* Invoke the function. *)
    Printf.sprintf "@%s" (get_function_label function_name) ;
    "0;JMP" ;

    Printf.sprintf "(%s)" return_label ;
  ]

(** Translates return command. *)
let translate_return : string list =
  (* Points register A to a scratch register. *)
  let at_scratch_register = "@R13" in
  (* Pops from scratch register into register D as if the scratch register
     points to the top of a stack. *)
  let pop_from_scratch_register : string =
    concat_commands
      [
        at_scratch_register ;
        "M=M-1" ;
        "A=M" ;
        "D=M" ;
      ]
  in
  let restore_segment (segment : string) : string =
    concat_commands
      [
        pop_from_scratch_register;
        Printf.sprintf "@%s" segment ;
        "M=D" ;
      ]
  in
  [
    (* Point scratch register to previous stack frame. *)
    "@LCL" ;
    "D=M" ;
    at_scratch_register ;
    "M=D" ;

    (* Save return address, which may be overwritten by the return value. *)
    "@5" ;
    "D=A" ;
    "@LCL" ;
    "A=M-D" ;
    "D=M" ;
    "@R14" ;  (* second scratch register *)
    "M=D" ;

    (* Write return value. *)
    pop_stack_to_d_register ;
    "@ARG" ;
    "A=M" ;
    "M=D" ;

    (* Restore stack pointer. *)
    "@ARG" ;
    "D=M" ;
    "@SP" ;
    "M=D+1" ;

    (* Restore memory segments. *)
    restore_segment "THAT" ;
    restore_segment "THIS" ;
    restore_segment "ARG" ;
    restore_segment "LCL" ;

    (* Go to return address. *)
    "@R14" ;
    "A=M" ;
    "0;JMP" ;
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
        | Function (function_name, num_local_variables)
          -> translate_function function_name num_local_variables
        | Call (function_name, num_arguments)
          -> translate_call next_state function_name num_arguments
        | Return -> translate_return
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

(** Bootstrapping code for the beginning of a translated program. *)
let assembly_preamble : string list =
  let state = TranslatorState.create "__preamble" in
  List.concat
    [
      [
        (* Initialize stack pointer *)
        "@256" ;
        "D=A" ;
        "@SP";
        "M=D" ;
      ] ;
      translate_call state "Sys.init" 0 ;
    ]

(** Runs the VM translator on the input path. *)
let run_translator (input_path : string) : unit =
  let input_files = get_input_files input_path in
  let output_filename = get_asm_output_filename input_path in
  let translation = List.(input_files >>= translate_file) in
  Out_channel.with_file output_filename ~f:(fun output_channel ->
      Out_channel.output_lines output_channel assembly_preamble ;
      Out_channel.output_lines output_channel translation)

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Translates a VM program to Hack assembly code."
    [%map_open
      let input_path = anon ("INPUT_PATH" %: string) in
      fun () -> run_translator input_path
    ]

let () = Command.run command
