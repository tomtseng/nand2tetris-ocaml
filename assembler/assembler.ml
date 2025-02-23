open Core
open Stdio

(** Table that maps program symbols to their addresses. *)
module SymbolTable : sig
  type t

  (** Creates a table populated with default symbols. *)
  val create : unit -> t

  (** Adds (symbol, value) to the table. *)
  val add : t -> symbol:string -> value:int -> [ `Ok | `Duplicate ]
  val add_exn : t -> symbol:string -> value:int -> unit

  (** [find table symbol] Finds the value in the table associated with the
      symbol. *)
  val find : t -> string -> int option
end = struct
  type t = (string, int) Hashtbl.t

  let create () =
    let table = Hashtbl.of_alist_exn (module String) [
        ("SP", 0) ;
        ("LCL", 1) ;
        ("ARG", 2) ;
        ("THIS", 3) ;
        ("THAT", 4) ;
        ("SCREEN", 16384) ;
        ("KBD", 24576) ]
    in
    for i = 0 to 15 do
      Hashtbl.add_exn table ~key:("R" ^ (string_of_int i)) ~data:i
    done;
    table

  let add table ~symbol ~value = Hashtbl.add table ~key:symbol ~data:value
  let add_exn table ~symbol ~value = Hashtbl.add_exn table ~key:symbol ~data:value

  let find table symbol = Hashtbl.find table symbol
end

(** Converts a non-negative integer to 15-bit binary form. *)
let int_to_binary (integer : int) : string =
  if integer < 0
  then failwith ("Unexpected negative integer: " ^ (string_of_int integer))
  else
    let rec int_to_binary_impl (i : int) (acc : int list) : int list =
      match i with
      | 0 -> acc
      | _ -> int_to_binary_impl (i / 2) ((i mod 2) :: acc)
    in
    let binary_list = int_to_binary_impl integer [] in
    let num_bits = 15 in
    let num_leading_zeros = num_bits - (List.length binary_list) in
    if num_leading_zeros < 0
    then
      failwith ("Integer larger than " ^ (string_of_int num_bits) ^ " bits: "
                ^ (string_of_int integer))
    else
      (String.make num_leading_zeros '0')
      ^ (String.concat (List.map binary_list ~f:string_of_int))

(** Returns whether the expression contains Ast_types.Memory_at_A *)
let rec does_expression_use_memory_at_a (e : Ast_types.expression) : bool =
  match e with
  | Ast_types.Memory Ast_types.Memory_at_A -> true
  | Ast_types.Memory _ -> false
  | Ast_types.Int _ -> false
  | Ast_types.Bit_negation e2 | Ast_types.Negative e2
    -> does_expression_use_memory_at_a e2
  | Ast_types.Operator (_, e1, e2) ->
    does_expression_use_memory_at_a e1 ||
    does_expression_use_memory_at_a e2

(** Translates a C-instruction computation to its binary representation. *)
let computation_to_binary (exp : Ast_types.expression) : string =
  let module A = Ast_types in
  let leading_bit = if does_expression_use_memory_at_a exp then "1" else "0" in
  let rec computation_to_binary_impl (e : A.expression) : string =
    match e with
    | A.Int 0 -> "101010"
    | A.Int 1 -> "111111"
    | A.Memory A.D_register -> "001100"
    | A.Memory (A.A_register | A.Memory_at_A) -> "110000"
    | A.Negative (A.Int 1) -> "111010"
    | A.Negative (A.Memory m) ->
      let bin = computation_to_binary_impl (A.Memory m) in
      (* Set last two bits. *)
      (String.drop_suffix bin 2) ^ "2"
    | A.Bit_negation (A.Memory m) ->
      let bin = computation_to_binary_impl (A.Memory m) in
      (* Set last bit. *)
      (String.drop_suffix bin 1) ^ "1"
    | A.Operator (A.Plus, (A.Memory A.D_register), (A.Int 1)) -> "011111"
    | A.Operator (A.Plus, (A.Memory (A.A_register | A.Memory_at_A)), (A.Int 1))
      -> "110111"
    | A.Operator (A.Minus, (A.Memory A.D_register), (A.Int 1)) -> "001110"
    | A.Operator (A.Minus, (A.Memory (A.A_register | A.Memory_at_A)), (A.Int 1))
      -> "110010"
    | A.Operator (A.Plus, (A.Memory A.D_register),
                  (A.Memory (A.A_register | A.Memory_at_A))) -> "000010"
    | A.Operator (A.Minus, (A.Memory A.D_register),
                  (A.Memory (A.A_register | A.Memory_at_A))) -> "010011"
    | A.Operator (A.Minus, (A.Memory (A.A_register | A.Memory_at_A)),
                  (A.Memory A.D_register)) -> "000111"
    | A.Operator (A.Bit_and, (A.Memory A.D_register),
                  (A.Memory (A.A_register | A.Memory_at_A))) -> "000000"
    | A.Operator (A.Bit_or, (A.Memory A.D_register),
                  (A.Memory (A.A_register | A.Memory_at_A))) -> "010101"
    | _ -> failwith
             (Printf.sprintf !"Unexpected expression %{sexp:A.expression}" e)
  in
  leading_bit ^ (computation_to_binary_impl exp)

(** Translates a C-instruction destination to its binary representation. *)
let destination_to_binary (dests : Ast_types.memory_location list) : string =
  let contains_dest (dest : Ast_types.memory_location) : char =
    if List.mem dests dest ~equal:Ast_types.equal_memory_location then '1'
    else '0'
  in
  String.of_char_list [
    contains_dest Ast_types.A_register;
    contains_dest Ast_types.D_register;
    contains_dest Ast_types.Memory_at_A]

(** Translates a C-instruction jump to its binary representation. *)
let jump_to_binary (jump_opt : Ast_types.jump_type option) : string =
  match jump_opt with
  | None -> "000"
  | Some jump ->
    match jump with
    | Ast_types.Jgt -> "001"
    | Ast_types.Jeq -> "010"
    | Ast_types.Jge -> "011"
    | Ast_types.Jlt -> "100"
    | Ast_types.Jne -> "101"
    | Ast_types.Jle -> "110"
    | Ast_types.Jmp -> "111"

(** [translate_a_instruction symbols instruction ~next_free_address] Translates
    A-instruction to binary code. [next_free_address] is the next free address
    to which a new variable symbol may be assigned.

    Returns binary code as well as whether a new variable symbol was defined in
    this instruction. *)
let translate_a_instruction
    (symbols : SymbolTable.t)
    (instruction : Ast_types.a_instruction)
    ~next_free_address:(next_free_address : int)
  : (string * [ `New_symbol | `No_new_symbol ]) =
  match instruction with
  | Set_to_int i -> ("0" ^ (int_to_binary i), `No_new_symbol)
  | Set_to_symbol sym ->
    match SymbolTable.find symbols sym with
    | Some address -> ("0" ^ (int_to_binary address), `No_new_symbol)
    | None ->
      let () = SymbolTable.add_exn symbols ~symbol:sym ~value:next_free_address in
      ("0" ^ (int_to_binary next_free_address), `New_symbol)

(** Translates C-instruction to binary code. *)
let translate_c_instruction (instruction : Ast_types.c_instruction) =
  String.concat [
    "111" ;
    computation_to_binary instruction.computation ;
    destination_to_binary instruction.destination ;
    jump_to_binary instruction.jump ]

(** Returns table with all label symbol definitions in the program. *)
let process_label_definitions
    (program : Ast_types.statement list)
    (symbols : SymbolTable.t)
  : unit =
  let rec process_labels_impl
      (remaining_program : Ast_types.statement list)
      (instruction_number : int)  (* Index of following instruction *)
    : unit =
    match remaining_program with
    | [] -> ()
    | (A_instruction _ | C_instruction _) :: p
      -> process_labels_impl p (instruction_number + 1)
    | (Label_definition sym) :: p ->
      match SymbolTable.add symbols ~symbol:sym ~value:instruction_number with
      | `Ok -> process_labels_impl p instruction_number
      | `Duplicate -> failwith ("Duplicate label: " ^ sym)
  in
  process_labels_impl program 0

(** Translate program from a list of statements to a list of binary instruction
    codes. *)
let translate_program (program : Ast_types.statement list) : string list =
  let symbols = SymbolTable.create () in
  let () = process_label_definitions program symbols in
  let rec translate_program_impl
      (remaining_program : Ast_types.statement list)
      (next_free_address : int)  (* Next free address for variable symbols *)
      (acc : string list)  (* Accumulated binary instruction codes in reverse order *)
    : string list =
    match remaining_program with
    | [] -> List.rev acc
    | (Label_definition _) :: p -> translate_program_impl p next_free_address acc
    | (C_instruction s) :: p ->
      translate_program_impl p next_free_address ((translate_c_instruction s) :: acc)
    | (A_instruction s) :: p ->
      match translate_a_instruction symbols s ~next_free_address:next_free_address with
      | (instruction, `No_new_symbol) ->
        translate_program_impl p next_free_address (instruction :: acc)
      | (instruction, `New_symbol) ->
        translate_program_impl p (next_free_address + 1) (instruction :: acc)
  in
  translate_program_impl program 16 []

(** Print the position information in `lexbuf` to `out_channel`. *)
let print_position (out_channel : Stdio.Out_channel.t) (lexbuf : Lexing.lexbuf) : unit =
  let p = lexbuf.lex_curr_p in
  fprintf out_channel "%s:%d:%d" p.pos_fname p.pos_lnum
    (p.pos_cnum - p.pos_bol + 1)

(** Parse the contents of lexbuf, printing any error that appears. *)
let parse_with_error (lexbuf : Lexing.lexbuf) : Ast_types.statement list =
  try Parser.program Lexer.read lexbuf with
  | Parser.Error ->
    let stack = Printexc.get_backtrace () in
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    fprintf stderr "%s" stack;
    exit (-1)

(** Returns `input_filename`, but with a ".hack" extension. *)
let get_output_filename (input_filename : string) : string =
  let
    base_filename, (_ : string option) = Filename.split_extension input_filename
  in
  base_filename ^ ".hack"

(** Runs the Hack assembler on the input file. **)
let run_assembler (input_filename : string) : unit =
  let lexbuf = Lexing.from_channel (In_channel.create input_filename) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_filename };
  let statements = parse_with_error lexbuf in
  let translation = translate_program statements in
  let output_filename = get_output_filename input_filename in
  Out_channel.with_file output_filename ~f:(fun output_channel ->
      Out_channel.output_lines output_channel translation)

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Translates a Hack assembly program to Hack machine code."
    [%map_open
      let input_filename = anon ("INPUT_FILENAME" %: string) in
      fun () -> run_assembler input_filename
    ]

let () = Command.run command
