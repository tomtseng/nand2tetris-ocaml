open Base

let memory_location_of_char (c : char) : Ast_types.memory_location =
  match c with
  | 'D' -> Ast_types.D_register
  | 'A' -> Ast_types.A_register
  | 'M' -> Ast_types.Memory_at_A
  | _ -> failwith ("Unexpected memory location: " ^ (String.of_char c))

let memory_location_of_string s =
  if (String.length s) <> 1
  then failwith ("Unexpected memory location: " ^ s)
  else memory_location_of_char (String.get s 0)

let memory_locations_of_string s =
  List.map (String.to_list s) ~f:memory_location_of_char

let jump_type_of_string s =
  match s with
  | "JGT" -> Ast_types.Jgt
  | "JEQ" -> Ast_types.Jeq
  | "JGE" -> Ast_types.Jge
  | "JLT" -> Ast_types.Jlt
  | "JNE" -> Ast_types.Jne
  | "JLE" -> Ast_types.Jle
  | "JMP" -> Ast_types.Jmp
  | _ -> failwith ("Unexpected jump type: " ^  s)
