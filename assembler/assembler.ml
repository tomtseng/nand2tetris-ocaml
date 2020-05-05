open Core
open Stdio

(** Returns `input_filename` but with a ".hack" extension. *)
let get_output_filename (input_filename : string) : string =
  let
    base_filename, (_ : string option) = Filename.split_extension input_filename
  in
  base_filename ^ ".hack"

(** Converts a non-negative integer to 15-bit binary form. *)
let int_to_binary (integer : int) : string =
  if integer < 0
  then failwith ("Unexpected negative integer: " ^ (string_of_int integer))
  else
    let rec int_to_binary_impl (i : int) (acc : int list) =
      match i with
      | 0 -> acc
      | _ -> int_to_binary_impl (i / 2) ((i mod 2) :: acc)
    in
    let binary_list = int_to_binary_impl integer [] in
    let num_bits = 15 in
    let num_leading_zeros = (List.length binary_list) - num_bits in
    if num_leading_zeros < 0
    then
      failwith ("Integer larger than " ^ (string_of_int num_bits) ^ " bits: "
        ^ (string_of_int integer))
    else
      (String.make num_leading_zeros '0')
        ^ (String.of_char_list (List.map binary_list ~f:char_of_int))

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
    | A.Int -1 -> "111010"
    | A.Memory A.D_register -> "001100"
    | A.Memory (A.A_register | A.Memory_at_A) -> "110000"
    | A.Bit_negation (A.Memory m) ->
        let bin = computation_to_binary_impl (A.Memory m) in
        (* Set last bit. *)
        (String.drop_suffix bin 1) ^ "1"
    | A.Negative (A.Memory m) ->
        let bin = computation_to_binary_impl (A.Memory m) in
        (* Set last two bits. *)
        (String.drop_suffix bin 2) ^ "2"
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
    | _ -> failwith "Unexpected expression"
  in
  leading_bit ^ (computation_to_binary_impl exp)

(** Translates a C-instruction destination to its binary representation. *)
let destination_to_binary (dests : Ast_types.memory_location list) : string =
  let contains_dest_to_bit (dest : Ast_types.memory_location) : char =
    if List.mem dests dest ~equal:Ast_types.equal_memory_location then '1'
    else '0'
  in
  String.of_char_list [
    contains_dest_to_bit Ast_types.A_register;
    contains_dest_to_bit Ast_types.Memory_at_A;
    contains_dest_to_bit Ast_types.D_register]

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

(** Translates A-instruction to binary code. *)
let translate_a_instruction (instruction : Ast_types.a_instruction) : string =
  match instruction with
  | Set_to_int i -> "0" ^ (int_to_binary i)
  | Set_to_symbol _ -> failwith "TODO not yet implemented"

(** Translates C-instruction to binary code. *)
let translate_c_instruction (instruction : Ast_types.c_instruction) =
  "111"
  ^ (computation_to_binary instruction.computation)
  ^ (destination_to_binary instruction.destination)
  ^ (jump_to_binary instruction.jump)

(** Translates statement to binary code. *)
let translate_statement (statement : Ast_types.statement) : string =
  match statement with
  | A_instruction s -> translate_a_instruction s
  | C_instruction s -> translate_c_instruction s
  | Symbol_definition sym -> "(TODO) New symbol: " ^ sym

(** Runs the Hack assembler on the input file. **)
let run_assembler (input_filename : string) : unit =
  let output_filename = get_output_filename input_filename in
  In_channel.with_file input_filename ~f:(fun input_channel ->
      Out_channel.with_file output_filename ~f:(fun output_channel ->
        let lexbuf = Lexing.from_channel input_channel in
        let statements = Parser.program Lexer.read lexbuf in
        Out_channel.output_lines output_channel
          (List.map statements ~f:translate_statement)
      ))

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Translates a Hack assembly program to Hack machine code."
    [%map_open
      let input_filename = anon ("INPUT_FILENAME" %: string) in
      fun () -> run_assembler input_filename
    ]

let () = Command.run command
